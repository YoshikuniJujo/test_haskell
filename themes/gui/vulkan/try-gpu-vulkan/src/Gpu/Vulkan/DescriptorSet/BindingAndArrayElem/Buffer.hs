{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-redundant-constraints #-}

module Gpu.Vulkan.DescriptorSet.BindingAndArrayElem.Buffer where

import GHC.TypeLits

import Gpu.Vulkan.Object qualified as VO
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Lyt

-- * BUFFER

-- ** BindingAndArrayElemBuffer

class BindingAndArrayElemBuffer
	(lbts :: [Lyt.BindingType]) (objs :: [VO.O]) (i :: Nat) where
	bindingAndArrayElemBuffer :: Integral n => n -> n -> (n, n)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Lyt.Buffer (VO.Static al 'Nothing ot t ': lobjs) ': lbts)
		(VO.Static al ('Just _nm) ot t ': objs) 0 where
	bindingAndArrayElemBuffer b ae = (b, ae)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Lyt.Buffer (VO.Static al ('Just _nm) ot t ': lobjs) ': lbts)
		(VO.Static al 'Nothing ot t ': objs) 0 where
	bindingAndArrayElemBuffer b ae = (b, ae)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Lyt.Buffer (VO.Dynamic n al 'Nothing ot t ': lobjs) ': lbts)
		(VO.Dynamic n al ('Just _nm) ot t ': objs) 0 where
	bindingAndArrayElemBuffer b ae = (b, ae)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Lyt.Buffer (VO.Dynamic n al ('Just _nm) ot t ': lobjs) ': lbts)
		(VO.Dynamic n al 'Nothing ot t ': objs) 0 where
	bindingAndArrayElemBuffer b ae = (b, ae)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Lyt.Buffer (obj ': lobjs) ': lbts) (obj ': objs) 0 where
	bindingAndArrayElemBuffer b ae = (b, ae)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBuffer ('Lyt.Buffer lobjs ': lbts)
		(VO.Static al ('Just nm) ot t ': objs) (i - 1) =>
	BindingAndArrayElemBuffer
		('Lyt.Buffer (VO.Static al 'Nothing ot t ': lobjs) ': lbts)
		(VO.Static al ('Just nm) ot t ': objs) i where
	bindingAndArrayElemBuffer b ae = bindingAndArrayElemBuffer
		@('Lyt.Buffer lobjs ': lbts)
		@(VO.Static al ('Just nm) ot t ': objs) @(i - 1) b (ae + 1)

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Lyt.Buffer lobjs ': lbts)
		(VO.Static al 'Nothing ot t ': objs) (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Lyt.Buffer (VO.Static al ('Just _nm) ot t ': lobjs) ': lbts)
		(VO.Static al 'Nothing ot t ': objs) i where
	bindingAndArrayElemBuffer b ae = bindingAndArrayElemBuffer
		@('Lyt.Buffer lobjs ': lbts)
		@(VO.Static al 'Nothing ot t ': objs) @(i - 1) b (ae + 1)

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Lyt.Buffer lobjs ': lbts)
		(VO.Dynamic n al ('Just nm) ot t ': objs) (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Lyt.Buffer (VO.Dynamic n al 'Nothing ot t ': lobjs) ': lbts)
		(VO.Dynamic n al ('Just nm) ot t ': objs) i where
	bindingAndArrayElemBuffer b ae =
		bindingAndArrayElemBuffer
			@('Lyt.Buffer lobjs ': lbts)
			@(VO.Dynamic n al ('Just nm) ot t ': objs) @(i - 1)
			b (ae + 1)

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Lyt.Buffer lobjs ': lbts)
		(VO.Dynamic n al 'Nothing ot t ': objs) (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Lyt.Buffer (VO.Dynamic n al ('Just _nm) ot t ': lobjs) ': lbts)
		(VO.Dynamic n al 'Nothing ot t ': objs) i where
	bindingAndArrayElemBuffer b ae =
		bindingAndArrayElemBuffer
			@('Lyt.Buffer lobjs ': lbts)
			@(VO.Dynamic n al 'Nothing ot t ': objs) @(i - 1)
			b (ae + 1)

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Lyt.Buffer lobjs ': lbts)
		(obj ': objs) (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Lyt.Buffer (obj ': lobjs) ': lbts) (obj ': objs) i where
	bindingAndArrayElemBuffer b ae =
		bindingAndArrayElemBuffer
			@('Lyt.Buffer lobjs ': lbts)
			@(obj ': objs) @(i - 1) b (ae + 1)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBuffer ('Lyt.Buffer lobjs ': lbts) (obj ': objs) i =>
	BindingAndArrayElemBuffer
		('Lyt.Buffer (lobj ': lobjs) ': lbts) (obj ': objs) i where
	bindingAndArrayElemBuffer b ae =
		bindingAndArrayElemBuffer
			@('Lyt.Buffer lobjs ': lbts)
			@(obj ': objs) @i b (ae + 1)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBuffer lbts (obj ': objs) i =>
	BindingAndArrayElemBuffer (bt ': lbts) (obj ': objs) i where
	bindingAndArrayElemBuffer b _ae =
		bindingAndArrayElemBuffer @lbts @(obj ': objs) @i (b + 1) 0

-- ** IsPrefixObject

class IsPrefixObject (objs :: [VO.O]) (objs' :: [VO.O])

instance IsPrefixObject '[] objs'

instance IsPrefixObject objs objs' => IsPrefixObject
	(VO.Static al 'Nothing ot t ': objs)
	(VO.Static al ('Just _nm) ot t ': objs')

instance IsPrefixObject objs objs' => IsPrefixObject
	(VO.Static al ('Just _nm) ot t ': objs)
	(VO.Static al 'Nothing ot t ': objs')

instance IsPrefixObject objs objs' => IsPrefixObject
	(VO.Dynamic n al 'Nothing ot t ': objs)
	(VO.Dynamic n al ('Just _nm) ot t ': objs')

instance IsPrefixObject objs objs' => IsPrefixObject
	(VO.Dynamic n al ('Just _nm) ot t ': objs)
	(VO.Dynamic n al 'Nothing ot t ': objs')

instance IsPrefixObject objs objs' =>
	IsPrefixObject (obj ': objs) (obj ': objs')
