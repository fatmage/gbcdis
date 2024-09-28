(lang dune 3.14)

(name gbcdis)

(generate_opam_files true)

(source
 (github fatmage/gbcdis))

(authors "Michał Bednarz (fatm4ge)")

(maintainers "Michał Bednarz (fatm4ge)")

(license MIT)

(documentation https://github.com/fatmage/gbcdis/blob/main/README.md)

(package
 (name gbcdis)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  ("gbc" "gameboy" "gameboy color" "disassembler" "disassembly")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
