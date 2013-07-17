open Hitscoreweb_std
open Html5

class head_cell
  (cell: Html5_types.span_content_fun Html5.elt list)
  (tooltip: string) =
object (self)
  method cell = cell
  method tooltip = tooltip
end
let head_text cell tooltip = new head_cell [pcdata cell] tooltip

let lane =
  head_text "Lane" "Lane number (1–8)"
let library_qn =
  head_text "Lib ID" "Project name '.' library name"
let library_name =
  head_text "Lib ID" "Library name"
let library_project =
  head_text "Project" "Project name"
let number_of_reads =
  head_text "# Reads" "Total number of reads"
let number_of_undetermined_reads =
  head_text "# Undetermined Reads" "Number of undetermined reads"
let percent_bases_over_q30 =
  head_text "% bases ≥ Q30" "Percentage of bases with Q-Score ≥ 30"
let zero_mismatch =
  head_text "% 0 Mismatch" "Percentage of reads demultiplexed with 0 mismatches"
let mean_qs =
  head_text "Mean QS (PF)" "Average Q-Score (for passed-filter reads)"

let library_description =
  head_text "Descr." "Library Short Description"

let seeding_concentration =
  head_text "Seeding C." "Seeding concentration"
let volume =
  head_text "Vol." "Volume"
let contacts_of_lane =
  head_text "Contacts" "Users associated with the lane"
let libraries_of_lane =
  head_text "Libraries" "Libraries of the lane"

let sample_name =
  head_text "Sample" "Sample name"
let organism_name =
  head_text "Organism/Source" "Name of the source/species of the sample"

let library_submissions =
  head_text "Submission(s)" "Flowcells/lanes in which the library has been submitted"

let library_application =
  head_text "Application" "Application (RNA-seq, DNA-seq, Chip-seq, …)"
let library_barcode = head_text "Barcode(s)" "Barcoding of the library"
let library_p5 = head_text "P5/A Lgth" "P5 (HiSeq) or A (PGM) Adapter Length"
let library_p7 = head_text "P7/P1 Lgth" "P7 (HiSeq) or P1 (PGM) Adapter Length"

let library_stranded = head_text "Stranded" "Stranded"
let library_truseq_control = head_text "TruSeq Ctrl" "TruSeq Control"
let library_rnaseq_control = head_text "RNASeq Ctrl" "RNA-Seq Control"
let library_preparator = head_text "Preparator" "Email of the library preparator"
let library_note = head_text "Note" "Note(s) about the library"

let bioanalyzer_well_nb = head_text "Bioanalyzer: Well NB" "Well Number"
let bioanalyzer_mean = head_text "Bioanalyzer: Mean" "Average Fragment Size"
let bioanalyzer_min = head_text "Bioanalyzer: Min" "Min Fragment Size"
let bioanalyzer_max = head_text "Bioanalyzer: Max" "Max Fragment Size"
let bioanalyzer_files = head_text "Bioanalyzer: Files" "Bioanalyzer Files"
let agarose_gel_well_nb = head_text "Agarose Gel: Well NB" "Well Number"
let agarose_gel_mean = head_text "Agarose Gel: Mean" "Average Fragment Size"
let agarose_gel_min = head_text "Agarose Gel: Min" "Min Fragment Size"
let agarose_gel_max = head_text "Agarose Gel: Max" "Max Fragment Size"
let agarose_gel_files = head_text "Agarose Gel: Files" "Agarose Gel Files"

let protocol = head_text "Protocol" "Protocol Description (if available)"

let demux_options =
  head_text "Demultiplexing Options" "Options used whie demultiplexing"
