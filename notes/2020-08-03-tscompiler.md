-- tag typescript compiler
-- title typescript compiler internal roadmap.
-- date 2020-08-03
-- source https://basarat.gitbook.io/typescript/overview
;;
# Typescript compiler internal roadmap.

#### Components
`Scanner` → `Parser` → `Binder` → `Checker` → `Emitter`.

Each component is located under the file with the same name. e.g `scanner.ts`. `Binder` generate symbol for more semantic analysis, `Checker` performs type checking. `Emitter` is a typescript to js code gen.

####
