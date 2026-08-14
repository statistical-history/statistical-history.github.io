# STA 119FS course website

Landing page for **Statistics as a Way of Thinking: History, Ideas, and Evidence**.

The site is deliberately dependency-free and is ready for GitHub Pages. The
main content lives in `index.html`; site-wide styles live in
`assets/css/main.css`.

## Preview locally

From the repository root, run:

```sh
python3 -m http.server 4000
```

Then open <http://localhost:4000>.

## Adding course material

The landing page already includes anchors for the course overview, guiding
questions, and materials. Replace the "Planned" states in the materials
section as pages become available. GitHub Pages will also render future
Markdown pages through Jekyll.
