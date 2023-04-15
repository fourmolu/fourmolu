# Configuration

Configuration options may be specified in either a `fourmolu.yaml` file or via command-line options. Fourmolu looks for a `fourmolu.yaml` file in all parents of the current directory, followed by [the XDG config directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:XdgConfig).

A configuration file with the default values filled in can be generated with:

```console
$$ fourmolu --default-config > fourmolu.yaml
```

For example, the `indentation` option may be specified in `fourmolu.yaml` as:

```yaml
indentation: 2
```

or on the command line as `--indentation 2` or `--indentation=2`.

Every option also has a setting that matches Ormolu's formatting.

## Available options

| Option name | Description |
|-------------|-------------|
$for(options)$| [`$name$`](/config/$name$) | $description$ |
$endfor$
