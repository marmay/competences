/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./frontend/**/*.hs",
    "./static/**/*.html",
    "./static/**/*.js"
  ],
  // Safelist ensures these classes are always included (important for Haskell string-based classes)
  safelist: [
    // Basecoat color palette - sky (primary)
    {
      pattern: /bg-sky-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /text-sky-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /border-sky-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /ring-sky-(50|100|200|300|400|500|600|700|800|900)/,
    },
    // Stone (secondary/neutral)
    {
      pattern: /bg-stone-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /text-stone-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /border-stone-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /ring-stone-(50|100|200|300|400|500|600|700|800|900)/,
    },
    // Red (destructive)
    {
      pattern: /bg-red-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /text-red-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /border-red-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /ring-red-(50|100|200|300|400|500|600|700|800|900)/,
    },
    // Other common colors used in the app
    {
      pattern: /bg-(gray|green|blue|yellow|orange)-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /text-(gray|green|blue|yellow|orange)-(50|100|200|300|400|500|600|700|800|900)/,
    },
    {
      pattern: /border-(gray|green|blue|yellow|orange)-(50|100|200|300|400|500|600|700|800|900)/,
    },
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
