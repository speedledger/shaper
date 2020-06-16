# Shaper - Super simple templAter

Shaper is a command line tool that helps bootstrapping simple, but repetative,
file additions in existing projects. A "shape" is defined in the project roots
`./.shapes/` directory and contains files with a simple `${tag}` style of string
replacement and a `shape.json` that describes default parameters and files to
ignore.

## Usage

Listing all available shapes is done with the `list` command.

Expanding a shape is done with the `realize` command. The command takes the name
of the shape, that is the name of the sub-directory in the `./.shapes/`
directory, and a list of parameters for the templates.

The necessary parameters for a template is listed with the `describe` command.

### Parameters

Parameters default value is given in the `./.shapes/<shapeName>/shape.json` file
under the `"params"` key. More parameters can be specified by piping in
json-content when calling `realize` with `--stdin true` and the the `--params`
argument.

Limitations:

* Parameters given with `--params` can not contain spaces or other characters
  that need escaping.

## Creating a Shape

* Create a directory with the shapes name in `.shapes/`
* Create a new file in the shape directory called `shape.json` containing:
  * `"params"` - A object with only string values of each parameter used in the
    template.
  * `"ignore"` - A list of strings of file paths that should not be included
    in the realization of the template.
* Create all the files with placeholders for param substitution
  * The placeholder syntax is `${keyName}` where `keyName` will be looked up in
    the params.
