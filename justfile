gen-readme:
    mkdir -p assets
    rm -rf assets/*
    spago run --main "Test.RunReadme"
    yarn purs-to-md test/Readme.purs README.md

format:
    purs-tidy format-in-place 'src/**/*.purs'