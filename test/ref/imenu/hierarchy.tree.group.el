(("axi_if_converter" . #<marker at 1057 in hierarchy.vhd>)
 ("RTL"
  ("*architecture_body*" . #<marker at 10723 in hierarchy.vhd>)
  ("Instances"
   ("axi_lite_regs" . #<marker at 15177 in hierarchy.vhd>)
   ("input_buffer" . #<marker at 17684 in hierarchy.vhd>)
   ("core_converter" . #<marker at 20906 in hierarchy.vhd>)
   ("core_converter" . #<marker at 25059 in hierarchy.vhd>)
   ("axi_lite_master" . #<marker at 29213 in hierarchy.vhd>)
   ("clk_div" . #<marker at 31113 in hierarchy.vhd>)
   ("clk_sync" . #<marker at 31411 in hierarchy.vhd>)
   ("pattern_counter" . #<marker at 31664 in hierarchy.vhd>)
   ("pattern_counter" . #<marker at 32245 in hierarchy.vhd>)
   ("core_fsm" . #<marker at 32826 in hierarchy.vhd>)
   ("core_fsm" . #<marker at 33650 in hierarchy.vhd>)))
 ("pattern_counter" . #<marker at 34613 in hierarchy.vhd>)
 ("RTL"
  ("*architecture_body*" . #<marker at 35214 in hierarchy.vhd>)
  ("Processes"
   ("pattern_count_proc" . #<marker at 35438 in hierarchy.vhd>)))
 ("clk_div" . #<marker at 36169 in hierarchy.vhd>)
 ("RTL"
  ("*architecture_body*" . #<marker at 36433 in hierarchy.vhd>)
  ("Processes"
   ("div_proc" . #<marker at 36552 in hierarchy.vhd>))
  ("Instances"
   ("BUFG" . #<marker at 36995 in hierarchy.vhd>)))
 ("clk_sync" . #<marker at 37205 in hierarchy.vhd>)
 ("RTL"
  ("*architecture_body*" . #<marker at 37446 in hierarchy.vhd>)
  ("Processes"
   ("clk_fs_sync_proc" . #<marker at 37525 in hierarchy.vhd>)))
 ("input_buffer" . #<marker at 38138 in hierarchy.vhd>)
 ("RTL"
  ("*architecture_body*" . #<marker at 40351 in hierarchy.vhd>)
  ("Components"
   ("blk_mem_gen_0" . #<marker at 40392 in hierarchy.vhd>))
  ("Procedures"
   ("bram_logic" . #<marker at 40969 in hierarchy.vhd>)
   ("bram_logic_rst" . #<marker at 41675 in hierarchy.vhd>)
   ("read_to_output_reg_logic" . #<marker at 42053 in hierarchy.vhd>)
   ("read_to_output_reg_logic_rst" . #<marker at 43499 in hierarchy.vhd>)
   ("bram_pointer_position_calc" . #<marker at 44216 in hierarchy.vhd>)
   ("bram_pointer_position_rst" . #<marker at 44699 in hierarchy.vhd>)
   ("load_output_reg" . #<marker at 45008 in hierarchy.vhd>)
   ("load_output_reg_rst" . #<marker at 46117 in hierarchy.vhd>))
  ("Processes"
   ("axi_bram_logic_l" . #<marker at 50816 in hierarchy.vhd>)
   ("axi_bram_logic_r" . #<marker at 51702 in hierarchy.vhd>)
   ("read_to_output_reg_l" . #<marker at 52588 in hierarchy.vhd>)
   ("read_to_output_reg_r" . #<marker at 53998 in hierarchy.vhd>)
   ("bram_ptr_pos_l_proc" . #<marker at 55408 in hierarchy.vhd>)
   ("bram_ptr_pos_r_proc" . #<marker at 56130 in hierarchy.vhd>)
   ("output_reg_loading_l" . #<marker at 56853 in hierarchy.vhd>)
   ("output_reg_loading_r" . #<marker at 58072 in hierarchy.vhd>)
   ("delay_output_reg_valid_l" . #<marker at 59290 in hierarchy.vhd>)
   ("delay_output_reg_valid_r" . #<marker at 59691 in hierarchy.vhd>))
  ("Instances"
   ("blk_mem_gen_0" . #<marker at 48671 in hierarchy.vhd>)
   ("blk_mem_gen_0" . #<marker at 49129 in hierarchy.vhd>)))
 ("core_fsm" . #<marker at 60224 in hierarchy.vhd>)
 ("RTL"
  ("*architecture_body*" . #<marker at 61028 in hierarchy.vhd>)
  ("Processes"
   ("fsm_axi_full" . #<marker at 61560 in hierarchy.vhd>)))
 ("core_converter" . #<marker at 64758 in hierarchy.vhd>)
 ("RTL"
  ("*architecture_body*" . #<marker at 69266 in hierarchy.vhd>)
  ("Functions"
   ("clogb2" . #<marker at 69309 in hierarchy.vhd>))
  ("Processes"
   ("fsm_proc" . #<marker at 76576 in hierarchy.vhd>)
   ("read_burst_size_calc_proc" . #<marker at 82084 in hierarchy.vhd>)
   ("araddr_proc" . #<marker at 83926 in hierarchy.vhd>)
   ("arvalid_proc" . #<marker at 84531 in hierarchy.vhd>)
   ("arlen_proc" . #<marker at 85478 in hierarchy.vhd>)
   ("rdata_counter_proc" . #<marker at 85835 in hierarchy.vhd>)
   ("rdata_total_counter_proc" . #<marker at 86311 in hierarchy.vhd>)
   ("r_done_proc" . #<marker at 86955 in hierarchy.vhd>)
   ("write_burst_size_calc_proc" . #<marker at 87409 in hierarchy.vhd>)
   ("awaddr_proc" . #<marker at 89364 in hierarchy.vhd>)
   ("strobe_burst_proc" . #<marker at 89968 in hierarchy.vhd>)
   ("awlen_proc" . #<marker at 91134 in hierarchy.vhd>)
   ("awvalid_proc" . #<marker at 91491 in hierarchy.vhd>)
   ("wdata_counter_proc" . #<marker at 92441 in hierarchy.vhd>)
   ("wdata_total_counter_proc" . #<marker at 92964 in hierarchy.vhd>)
   ("wlast_proc" . #<marker at 94162 in hierarchy.vhd>)
   ("bready_proc" . #<marker at 95408 in hierarchy.vhd>)
   ("request_edge_detection_proc" . #<marker at 96265 in hierarchy.vhd>)
   ("pattern_cnt_proc" . #<marker at 96676 in hierarchy.vhd>)))
 ("axi_lite_regs" . #<marker at 98535 in hierarchy.vhd>)
 ("RTL"
  ("*architecture_body*" . #<marker at 101110 in hierarchy.vhd>)
  ("Procedures"
   ("add_bit" . #<marker at 105604 in hierarchy.vhd>))
  ("Processes"
   ("axi_awready_proc" . #<marker at 106465 in hierarchy.vhd>)
   ("axi_awaddr_proc" . #<marker at 107109 in hierarchy.vhd>)
   ("axi_wready_proc" . #<marker at 107732 in hierarchy.vhd>)
   ("axi_bvalid_proc" . #<marker at 108531 in hierarchy.vhd>)
   ("axi_arready_proc" . #<marker at 109530 in hierarchy.vhd>)
   ("axi_rvalid_proc" . #<marker at 110619 in hierarchy.vhd>)
   ("axi_rdata_proc" . #<marker at 111278 in hierarchy.vhd>)
   ("address_decoding_proc" . #<marker at 111717 in hierarchy.vhd>)
   ("read_write_regs_proc" . #<marker at 113845 in hierarchy.vhd>)
   ("read_only_regs_proc" . #<marker at 118982 in hierarchy.vhd>)
   ("soft_reset_out_proc" . #<marker at 121332 in hierarchy.vhd>)
   ("mm2s_proc" . #<marker at 122005 in hierarchy.vhd>)
   ("master_lite_proc" . #<marker at 122656 in hierarchy.vhd>))
  ("control_reg_signals_proc"
   ("*process_statement*" . #<marker at 120721 in hierarchy.vhd>)
   ("Procedures"
    ("add_out_sigH" . #<marker at 120780 in hierarchy.vhd>))))
 ("axi_lite_master" . #<marker at 123512 in hierarchy.vhd>)
 ("RTL"
  ("*architecture_body*" . #<marker at 125536 in hierarchy.vhd>)
  ("Processes"
   ("awvalid_proc" . #<marker at 127058 in hierarchy.vhd>)
   ("wvalid_proc" . #<marker at 127554 in hierarchy.vhd>)
   ("awaddr_proc" . #<marker at 128043 in hierarchy.vhd>)
   ("wdata_proc" . #<marker at 128400 in hierarchy.vhd>)
   ("bready_proc" . #<marker at 128915 in hierarchy.vhd>)
   ("arvalid_proc" . #<marker at 129401 in hierarchy.vhd>)
   ("rready_proc" . #<marker at 129901 in hierarchy.vhd>)
   ("araddr_proc" . #<marker at 130387 in hierarchy.vhd>)
   ("do_read" . #<marker at 130742 in hierarchy.vhd>)
   ("error_proc" . #<marker at 131534 in hierarchy.vhd>))))