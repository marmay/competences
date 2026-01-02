/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./frontend/**/*.hs",
    "./static/**/*.html",
    "./static/**/*.js"
  ],
  // Safelist ensures these classes are always included (important for Haskell string-based classes)
  safelist: [
    // Semantic color classes (using CSS variables)
    { pattern: /bg-(primary|secondary|destructive|background|card|muted|accent)/ },
    { pattern: /text-(primary|secondary|destructive|foreground|muted|accent)/ },
    { pattern: /border-(primary|secondary|destructive|border|input|accent)/ },
    { pattern: /ring-(primary|ring|accent)/ },
    // Ability colors (domain-specific, using CSS variables)
    { pattern: /bg-ability-(success|success-light|warning|danger)/ },
    { pattern: /text-ability-(success|success-light|warning|danger)/ },
    // Keep some raw Tailwind colors for backwards compat during migration
    { pattern: /bg-(green|amber|red)-(500|600|700)/ },
    { pattern: /text-(green|amber|red)-(500|600|700)/ },
    // Other common colors used in the app
    { pattern: /bg-(gray|green|blue|yellow|orange|emerald|teal|rose)-(50|100|200|300|400|500|600|700|800|900)/ },
    { pattern: /text-(gray|green|blue|yellow|orange|emerald|teal|rose)-(50|100|200|300|400|500|600|700|800|900)/ },
    { pattern: /border-(gray|green|blue|yellow|orange)-(50|100|200|300|400|500|600|700|800|900)/ },
  ],
  theme: {
    extend: {
      colors: {
        // Semantic colors using Basecoat CSS variables
        primary: {
          DEFAULT: 'oklch(var(--primary) / <alpha-value>)',
          foreground: 'oklch(var(--primary-foreground) / <alpha-value>)',
        },
        secondary: {
          DEFAULT: 'oklch(var(--secondary) / <alpha-value>)',
          foreground: 'oklch(var(--secondary-foreground) / <alpha-value>)',
        },
        destructive: {
          DEFAULT: 'oklch(var(--destructive) / <alpha-value>)',
          foreground: 'oklch(var(--destructive-foreground) / <alpha-value>)',
        },
        muted: {
          DEFAULT: 'oklch(var(--muted) / <alpha-value>)',
          foreground: 'oklch(var(--muted-foreground) / <alpha-value>)',
        },
        accent: {
          DEFAULT: 'oklch(var(--accent) / <alpha-value>)',
          foreground: 'oklch(var(--accent-foreground) / <alpha-value>)',
        },
        background: 'oklch(var(--background) / <alpha-value>)',
        foreground: 'oklch(var(--foreground) / <alpha-value>)',
        card: {
          DEFAULT: 'oklch(var(--card) / <alpha-value>)',
          foreground: 'oklch(var(--card-foreground) / <alpha-value>)',
        },
        popover: {
          DEFAULT: 'oklch(var(--popover) / <alpha-value>)',
          foreground: 'oklch(var(--popover-foreground) / <alpha-value>)',
        },
        border: 'oklch(var(--border) / <alpha-value>)',
        input: 'oklch(var(--input) / <alpha-value>)',
        ring: 'oklch(var(--ring) / <alpha-value>)',
        // Ability colors (domain-specific, themable)
        ability: {
          success: 'oklch(var(--ability-success) / <alpha-value>)',
          'success-light': 'oklch(var(--ability-success-light) / <alpha-value>)',
          warning: 'oklch(var(--ability-warning) / <alpha-value>)',
          danger: 'oklch(var(--ability-danger) / <alpha-value>)',
        },
      },
      borderRadius: {
        lg: 'var(--radius)',
        md: 'calc(var(--radius) - 2px)',
        sm: 'calc(var(--radius) - 4px)',
      },
    },
  },
  plugins: [],
}
