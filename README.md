## Description
A tool to export your local Logseq Graph including all Assets and Excalidraw Diagrams to (Hugo) Markdown files.

<p align="right">(<a href="#top">back to top</a>)</p>

## Setup

Install `logseq-to-markdown` from npm:

`npm install @dom8509/logseq-to-markdown -g`

Omit `-g` for a local install.

<p align="right">(<a href="#top">back to top</a>)</p>

## Usage

```sh
Export your local Logseq Graph to (Hugo) Markdown files.

Usage: logseq-to-markdown [options] graph

Options:
  -o, --outputdir PATH                     ./out                Output Directory
  -e, --excluded-properties PROPERTY_LIST  #{:filters :public}  Comma separated list of properties that should be ignored
  -n, --trim-namespaces                                         Trim Namespace Names
  -b, --keep-bullets                                            Keep Outliner Bullets
  -t, --export-tasks                                            Export Logseq Tasks
  -a, --export-all                                              Export all Logseq Pages
  -r, --rm-brackets                                             Remove Link Brackets
  -p, --prerender-diagrams                                      Prerender Diagrams and export images
  -d, --delete-outputdir                                        Delete output directory before exporting data
      --time-pattern PATTERN               yyyy-MM-dd           Template Pattern for Time Strings
  -v, --verbose                                                 Verbose Output
  -h, --help

Graph: Name of the Logseq Graph

Please refer to the manual page for more information.
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Website templates

- 🏡 [hugo-PaperMod](https://github.com/dom8509/hugo-PaperMod) - My modified website template to render the Markdown pages exported by logseq-to-markdown. You can use this to host your personal website with free GitHub pages.

<p align="right">(<a href="#top">back to top</a>)</p>

## Issues

See the [open issues](https://github.com/dom8509/logseq-to-markdown/issues) for a full list of proposed features (and known issues).

### What works

- Internal and external Links
- Block References
- Youtube Videos
- Org Mode Commands
- Images
- Excalidraw Diagrams
- Doc As Code Diagrams
- EChart Diagrams
- Namspaces
- Prerendering Diagrams

### What is known to _not_ work

- Embedded Blocks
- Queries

<p align="right">(<a href="#top">back to top</a>)</p>

## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>

## Credits
* 🔥 [logseq-schrodinger](https://github.com/sawhney17/logseq-schrodinger) - For the inspiration to create this script and making a digital garden with Logseq and Hugo.
* 🪵 [nbb-logseq](https://github.com/logseq/nbb-logseq) - For making it possible to create Node.js ClojureScript Scripts to access the Logseq graph.