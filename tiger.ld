
SECTIONS {
 .rodata : {
   . =  (. + 3) & ~ 3;
   pc_map_start = .;
   *(.pcmap)
   pc_map_len = .;
   LONG(. - pc_map_start);
   *(.pcmap_data)
 }
}
