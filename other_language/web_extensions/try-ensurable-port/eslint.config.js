import globals from "globals";

export default [
	{
		files: ["**/*.js"],
		languageOptions: {
			globals: {
				...globals.browser,
				browser: "readonly",
				cloneInto: "readonly",
				URLPattern: "readonly"
			}
		},
		rules: {
			"no-unused-vars": "warn",
			"no-undef": "error"
		}
	}
];
