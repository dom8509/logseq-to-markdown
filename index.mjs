#!/usr/bin/env node

import { addClassPath, loadFile } from '@dom8509/nbb-logseq';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';

const __dirname = fileURLToPath(dirname(import.meta.url));

addClassPath(resolve(__dirname, 'src'));
await loadFile(resolve(__dirname, 'src/logseq-exporter.cljs'));
