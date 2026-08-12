/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["index.html", "*.ml", "index.js"],
  theme: {
    extend: {},
  },
  plugins: [require("@tailwindcss/typography")],
};
