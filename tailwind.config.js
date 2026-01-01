/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./frontend/**/*.hs",
    "./static/**/*.html",
    "./static/**/*.js"
  ],
  theme: {
    extend: {
      // Basecoat-inspired color palette
      colors: {
        // Using Tailwind's built-in colors with semantic names
        // primary: sky (already in Tailwind)
        // secondary: stone (already in Tailwind)
        // destructive: red (already in Tailwind)
      },
      // Custom spacing values matching Basecoat if needed
      spacing: {
        // Tailwind's default spacing already covers Basecoat's scale
      },
    },
  },
  plugins: [],
}
