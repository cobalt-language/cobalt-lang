//! ob

use std::collections::HashMap;

use object::{self, write, Object, ObjectComdat, ObjectSection, ObjectSymbol};

pub fn get_writeable_object_from_file(in_object: object::File) -> object::write::Object {
    let mut out_object = object::write::Object::new(
        in_object.format(),
        in_object.architecture(),
        in_object.endianness(),
    );
    out_object.mangling = object::write::Mangling::None;
    out_object.flags = in_object.flags();

    // Need to copy over:
    // - sections
    // - symbols

    let mut out_sections = HashMap::new();
    for in_section in in_object.sections() {
        if in_section.kind() == object::SectionKind::Metadata {
            continue;
        }

        let section_id = out_object.add_section(
            in_section
                .segment_name()
                .unwrap()
                .unwrap_or("")
                .as_bytes()
                .to_vec(),
            in_section.name().unwrap().as_bytes().to_vec(),
            in_section.kind(),
        );

        let out_section = out_object.section_mut(section_id);
        if out_section.is_bss() {
            out_section.append_bss(in_section.size(), in_section.align());
        } else {
            out_section.set_data(in_section.data().unwrap(), in_section.align());
        }
        out_section.flags = in_section.flags();

        out_sections.insert(in_section.index(), section_id);
    }

    let mut out_symbols = HashMap::new();
    for in_symbol in in_object.symbols() {
        if in_symbol.kind() == object::SymbolKind::Null {
            continue;
        }

        let (section, value) = match in_symbol.section() {
            object::SymbolSection::None => {
                (object::write::SymbolSection::None, in_symbol.address())
            }
            object::SymbolSection::Undefined => {
                (object::write::SymbolSection::Undefined, in_symbol.address())
            }
            object::SymbolSection::Absolute => {
                (object::write::SymbolSection::Absolute, in_symbol.address())
            }
            object::SymbolSection::Common => {
                (object::write::SymbolSection::Common, in_symbol.address())
            }
            object::SymbolSection::Section(index) => {
                if let Some(out_section) = out_sections.get(&index) {
                    (
                        object::write::SymbolSection::Section(*out_section),
                        in_symbol.address() - in_object.section_by_index(index).unwrap().address(),
                    )
                } else {
                    // Ignore symbols for sections that we have skipped
                    assert_eq!(in_symbol.kind(), object::SymbolKind::Section);
                    continue;
                }
            }
            _ => panic!("unknown symbol section for {:?}", in_symbol),
        };

        let flags = match in_symbol.flags() {
            object::SymbolFlags::None => object::SymbolFlags::None,
            object::SymbolFlags::Elf { st_info, st_other } => {
                object::SymbolFlags::Elf { st_info, st_other }
            }
            object::SymbolFlags::MachO { n_desc } => object::SymbolFlags::MachO { n_desc },
            object::SymbolFlags::CoffSection {
                selection,
                associative_section,
            } => {
                let associative_section =
                    associative_section.map(|index| *out_sections.get(&index).unwrap());
                object::SymbolFlags::CoffSection {
                    selection,
                    associative_section,
                }
            }
            object::SymbolFlags::Xcoff {
                n_sclass,
                x_smtyp,
                x_smclas,
                containing_csect,
            } => {
                let containing_csect =
                    containing_csect.map(|index| *out_symbols.get(&index).unwrap());
                object::SymbolFlags::Xcoff {
                    n_sclass,
                    x_smtyp,
                    x_smclas,
                    containing_csect,
                }
            }
            _ => panic!("unknown symbol flags for {:?}", in_symbol),
        };

        let out_symbol = object::write::Symbol {
            name: in_symbol.name().unwrap_or("").as_bytes().to_vec(),
            value,
            size: in_symbol.size(),
            kind: in_symbol.kind(),
            scope: in_symbol.scope(),
            weak: in_symbol.is_weak(),
            section,
            flags,
        };

        let symbol_id = out_object.add_symbol(out_symbol);
        out_symbols.insert(in_symbol.index(), symbol_id);
    }

    for in_section in in_object.sections() {
        if in_section.kind() == object::SectionKind::Metadata {
            continue;
        }
        let out_section = *out_sections.get(&in_section.index()).unwrap();
        for (offset, in_relocation) in in_section.relocations() {
            let symbol = match in_relocation.target() {
                object::RelocationTarget::Symbol(symbol) => *out_symbols.get(&symbol).unwrap(),
                object::RelocationTarget::Section(section) => {
                    out_object.section_symbol(*out_sections.get(&section).unwrap())
                }
                _ => panic!("unknown relocation target for {:?}", in_relocation),
            };
            let out_relocation = write::Relocation {
                offset,
                size: in_relocation.size(),
                kind: in_relocation.kind(),
                encoding: in_relocation.encoding(),
                symbol,
                addend: in_relocation.addend(),
            };
            out_object
                .add_relocation(out_section, out_relocation)
                .unwrap();
        }
    }

    for in_comdat in in_object.comdats() {
        let mut sections = Vec::new();
        for in_section in in_comdat.sections() {
            sections.push(*out_sections.get(&in_section).unwrap());
        }
        out_object.add_comdat(write::Comdat {
            kind: in_comdat.kind(),
            symbol: *out_symbols.get(&in_comdat.symbol()).unwrap(),
            sections,
        });
    }

    out_object
}
