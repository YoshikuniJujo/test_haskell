import { Hono } from "hono";
import { serve } from "@hono/node-server";

const app = new Hono();

app.get("/", c => {
	console.log("get /");
	return c.text("hello\n"); });

serve(app);
