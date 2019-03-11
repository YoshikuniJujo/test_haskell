{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Parts (
	-- * API
	updateNeighborType, getMonthlyFee, addMonthlyFee, getAmount, settle,
	-- * Types
	Phase(..), NeighborId, NeighborName, NeighborTypeId, MonthlyFee, Amount
	) where

import Control.Monad.IO.Class
import Data.Functor.ProductIsomorphic
import Data.Int
import Data.String
import Data.Time
import System.Random
import Text.Nowdoc
import Database.Relational
import Database.HDBC
import Database.HDBC.Record
import GmoPg hiding (Amount)

import qualified Data.Maybe as Mb

import Lib
import Neighbors as Nbs
import NeighborTypes as Nts
import Purchases
import NeighborTypeOperations (neighborTypeOperations)
import qualified NeighborTypeOperations as Ntos

updateNeighborType :: IConnection conn => conn -> ZonedTime -> IO ()
updateNeighborType conn zt =
	() <$ runUpdate conn (updateNeighborTypeUpdate zt) ()
--	runRaw conn $ rawNeighborTypeUpdate zt

updateNeighborTypeUpdate :: ZonedTime -> Update ()
updateNeighborTypeUpdate zt | timeZoneName (zonedTimeZone zt) /= "JST" =
	error "Time zone should be `JST'"
updateNeighborTypeUpdate zt_ = update $ \proj -> do
	neighborTypeId' <-# fromMaybe
		(proj ! neighborTypeId') (proj ! newNeighborTypeId')
	newNeighborTypeId' <-# value Nothing
--	neighborTypeUpdatedAt' <-# value Nothing
--	nbtos <- query neighborTypeOperations
--	on $ proj ! Nbs.id' .=. nbtos ! Ntos.neighborId'
--	wheres . isJust $ proj ! neighborTypeUpdatedAt'
--	wheres $ nbtos ! Ntos.createdAt' .<.  value (zonedTimeToLocalTime zt)

--	wheres . isJust $ proj ! newNeighborTypeId'
	tl <- queryList . relation $ do
		t <- query neighborTypeOperations
		wheres $ t ! Ntos.neighborId' .=. proj ! Nbs.id'
		wheres $ t ! Ntos.createdAt' .<. value (zonedTimeToLocalTime zt)
		return (value (1 :: Int64))
	wheres $ exists tl

	return unitPlaceHolder
	where zt = monthHead zt_

rawNeighborTypeUpdate :: ZonedTime -> String
rawNeighborTypeUpdate zt_ = [nowdoc|
	UPDATE hafh_db.neighbors nbs JOIN hafh_db.neighbor_type_operations ntos
		ON nbs.id = ntos.neighbor_id
		SET	nbs.neighbor_type_id = CASE
				WHEN (nbs.new_neighbor_type_id IS NULL)
					THEN nbs.neighbor_type_id
					ELSE nbs.new_neighbor_type_id END,
			nbs.new_neighbor_type_id = NULL
		WHERE	(NOT (nbs.new_neighbor_type_id IS NULL)) AND
			(ntos.created_at < TIMESTAMP '|] ++
				show (zonedTimeToLocalTime zt) ++ "')"
	where zt = monthHead zt_

type NeighborName = String
type NeighborTypeId = Int64
type MonthlyFee = Int32

monthHead :: ZonedTime -> ZonedTime
monthHead zt@ZonedTime { zonedTimeToLocalTime = lt } = zt {
	zonedTimeToLocalTime = LocalTime {
		localDay = monthHeadDay $ localDay lt,
		localTimeOfDay = midnight } }
	where
	monthHeadDay d = let (y, m, _d) = toGregorian d in fromGregorian y m 1

getMonthlyFee :: IConnection conn =>
	conn -> IO [(NeighborId, NeighborTypeId, MonthlyFee)]
getMonthlyFee conn = runQuery' conn (relationalQuery getMonthlyFeeGen) ()

getMonthlyFeeGen :: Relation () (NeighborId, NeighborTypeId, MonthlyFee)
getMonthlyFeeGen = relation $ do
	a <- query neighbors
	b <- query neighborTypes
	on $ a ! neighborTypeId' .=. b ! Nts.id'
	return $ (,,)
		|$| a ! Nbs.id'
		|*| a ! neighborTypeId'
		|*| b ! monthlyFee'

addMonthlyFee :: IConnection conn => conn -> ZonedTime -> Maybe Int32 ->
	NeighborId -> NeighborTypeId -> MonthlyFee -> IO ()
addMonthlyFee conn zt pid ni nti mf = () <$ runInsert
	conn (insertPurchase pid (fromIntegral $ - nti) ni mf zt)
		()

getAmount :: IConnection conn => conn -> IO [(NeighborId, Amount)]
getAmount conn = do
	nas <- runQuery' conn (relationalQuery getPrices) ()
	return . (<$> nas) $ \(nid, amnt) -> (nid, Mb.fromMaybe
		(error $ "Can't get amount of neighbor : " ++ show nid)
		amnt)
	where
	getPrices = aggregateRelation $ do
		p <- query purchases
		g <- groupBy $ p ! neighborId'
	--	asc $ p ! neighborId'
		return $ (,) |$| g |*| sum' (p ! price')

type NeighborId = Int64
type Amount = Int32

data Phase = Monthly | Weekly1 | Weekly2 | Weekly3 | Weekly4 deriving Show

settle :: IConnection conn =>
	Phase -> conn -> ZonedTime -> NeighborId -> Amount -> GmoPgM ()
settle ph conn ct nid amnt = do
	liftIO $ do
		_ <- runUpdate conn (updateUnsettledFlag nid (- 1)) ()
		_ <- runUpdate conn (updateSettlementFlag nid (- 1)) ()
		commit conn
	rslt3 <- tryG $ takeMoney nid amnt
	case rslt3 of
		Left _ -> liftIO $ do
			_ <- runUpdate conn (updateUnsettledFlag nid 1) ()
			_ <- runUpdate conn (updateSettlementFlag nid 2) ()
			commit conn
		Right _ -> liftIO $ do
			_ <- runUpdate conn (updateUnsettledFlag nid 0) ()
			_ <- runUpdate conn (updateSettlementFlag nid 1) ()
			_ <- runUpdate conn (updateSettlementDate nid ct) ()
			commit conn

takeMoney :: NeighborId -> Int32 -> GmoPgM ()
takeMoney nid amnt = do
	oi <- liftIO getDummyOrderId
	capture oi (mkMemberId nid) 0 LumpSum (fromIntegral amnt)

getDummyOrderId :: IO OrderId
getDummyOrderId = fromString . take 27
	. ("dummyOrderId-" <>) . show . abs <$> (randomIO :: IO Int)

updateUnsettledFlag :: NeighborId -> Int32 -> Update ()
updateUnsettledFlag nid uf = update $ \proj -> do
	unsettledFlag' <-# value uf
	wheres $ proj ! Nbs.id' .=. value nid
	return unitPlaceHolder

updateSettlementFlag :: NeighborId -> Int16 -> Update ()
updateSettlementFlag nid uf = update $ \proj -> do
	settlementFlag' <-# value uf
	wheres $ proj ! neighborId' .=. value nid
	return unitPlaceHolder

updateSettlementDate :: NeighborId -> ZonedTime -> Update ()
updateSettlementDate nid zt = update $ \proj -> do
	settlementDate' <-# value (Just . localDay $ zonedTimeToLocalTime zt)
	wheres $ proj ! neighborId' .=. value nid
	return unitPlaceHolder
