Utilities for transforming data file formats:

* `csv2dsv` Transform CSV to/from "delimiter separated values" where
  delimeter is the pipe symbol ('|').  Works well, except when certain
  non-printing characters appear in the input (not sure which).


Other work in progress:

* `csvtoxml` Transform CSV to a two-level equivalent XML format, where
  the top level represents rows, and the row children represent
  cells.  Incomplete (do not use).
* `xmltocsv` Transform two level XML to CSV.  Incomplete (do not use).
* `yaml2xml` Attempt to transform YAML into XML.  Incomplete (do not use).
