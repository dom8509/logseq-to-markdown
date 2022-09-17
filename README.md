## Description
A tool to export your local Logseq Graph including all Assets and Excalidraw Diagrams to (Hugo) Markdown files.

## Setup

Install `logset-exporter` from npm:

`npm install @dom8509/logseq-exporter -g`

Omit `-g` for a local install.

## Usage

```sh
Export your local Logseq Graph to (Hugo) Markdown files.

Usage: logseq-exporter [options] graph

Options:
  -e, --excluded-properties PATH  #{:filters :public}  Comma separated list of properties that should be ignored
  -n, --trim-namespaces                                Trim Namespace Names
  -b, --keep-bullets                                   Keep Outliner Bullets
  -t, --export-tasks                                   Export Logseq Tasks
  -r, --rm-brackets                                    Remove Link Brackets
  -o, --outputdir PATH            ./out                Output Directory
  -v, --verbose                                        Verbose Output
  -h, --help

Graph: Name of the Logseq Graph

Please refer to the manual page for more information.
```
