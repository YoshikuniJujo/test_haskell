import js from "@eslint/js";
import globals from "globals";

export default [ {
	"files": ["**/*.js"],
	"languageOptions": {
		ecmaVersion: 2021,
		sourceType: "module",
		globals: {
			...globals.browser,
			browser: "readonly",
			cloneInto: "readonly"
		}
	},
	rules: {
		...js.configs.recommended.rules
	}
} ];
