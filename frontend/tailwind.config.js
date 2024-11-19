/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["index.html", "*.ml"],
  theme: {
    extend: {},
  },
  plugins: [require('@tailwindcss/typography')],
};
