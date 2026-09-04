import "fake-indexeddb/auto";
import { addUser, login } from "../src/login.js"

console.log("FAKE FAKE FAKE");

await addUser(indexedDB, "tarou", "foobar");
console.log(await login(indexedDB, "tarou", "foobar"));
console.log(await login(indexedDB, "tarou", "foobarbaz"));
