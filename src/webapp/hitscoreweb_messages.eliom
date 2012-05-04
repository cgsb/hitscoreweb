open Hitscoreweb_std
open Html5
  
class head_cell
  (cell: HTML5_types.span_content_fun Html5.elt list)
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
let number_of_reads =
  head_text "# Reads" "Total umber of reads"
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
