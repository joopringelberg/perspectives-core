import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import sourcemaps from 'rollup-plugin-sourcemaps';
import url from '@rollup/plugin-url';
import del from 'rollup-plugin-delete';
import replace from '@rollup/plugin-replace';
import { promises as fs } from 'fs';

export default async function() {
  const packageJson = JSON.parse(await fs.readFile(new URL('./package.json', import.meta.url)));

  return {
    input: './output/Main/index.js', // Entry point
    output: {
      file: './dist/perspectives-core.js', // Output file
      format: 'es', // Output format (ES module)
      sourcemap: true, // Enable source maps
      name: 'perspectivesCore', // Global variable name for UMD/IIFE builds
    },
    plugins: [
      del({ targets: 'dist/*' }), // Clean up the dist directory
      resolve(
        {preferBuiltins: (module) => module !== 'events'} // Resolve built-in modules except 'events'
      ), // Resolve node_modules
      commonjs(), // Convert CommonJS modules to ES6
      json(), // Handle JSON files
      sourcemaps(), // Include existing source maps
      url({
        include: ['**/*.arc'], // Include .arc files
        limit: 0, // No limit, always include the file
        fileName: '[name][extname]', // Output file name pattern
      }),
      replace({
        preventAssignment: true,
        __PDRVersion__: JSON.stringify(packageJson.version ? packageJson.version : 'no version'),
        __MYCONTEXTS__: JSON.stringify('https://mycontexts.com/')
      }),
      // terser(), // Minify the output
    ],
    external: ['eventsource'], // External dependencies
  };
};