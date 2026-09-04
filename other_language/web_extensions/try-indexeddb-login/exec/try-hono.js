import "fake-indexeddb/auto";
import { Hono } from "hono";
import { serve } from "@hono/node-server";
import { addUser, login } from "../src/login.js"

const app = new Hono();

app.post("/add", async c => {
	const { uid, pswd } = await c.req.json();
	await addUser(indexedDB, uid, pswd);
	return c.json({ ok: true }); });

app.post("/login", async c => {
	const {uid, pswd } = await c.req.json();
	const ok = await login(indexedDB, uid, pswd);
	return c.json({ ok }); });

serve(app);
