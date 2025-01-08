(("axi_if_converter" . 1057)
 ("RTL"
  ("*architecture_body*" . 10723)
  ("Instances"
   ("axi_lite_regs" . 15177)
   ("input_buffer" . 17684)
   ("core_converter" . 20906)
   ("core_converter" . 25059)
   ("axi_lite_master" . 29213)
   ("clk_div" . 31113)
   ("clk_sync" . 31411)
   ("pattern_counter" . 31664)
   ("pattern_counter" . 32245)
   ("core_fsm" . 32826)
   ("core_fsm" . 33650)))
 ("pattern_counter" . 34613)
 ("RTL"
  ("*architecture_body*" . 35214)
  ("Processes"
   ("pattern_count_proc" . 35438)))
 ("clk_div" . 36169)
 ("RTL"
  ("*architecture_body*" . 36433)
  ("Processes"
   ("div_proc" . 36552))
  ("Instances"
   ("BUFG" . 36995)))
 ("clk_sync" . 37205)
 ("RTL"
  ("*architecture_body*" . 37446)
  ("Processes"
   ("clk_fs_sync_proc" . 37525)))
 ("input_buffer" . 38138)
 ("RTL"
  ("*architecture_body*" . 40351)
  ("Components"
   ("blk_mem_gen_0" . 40392))
  ("Procedures"
   ("bram_logic" . 40969)
   ("bram_logic_rst" . 41675)
   ("read_to_output_reg_logic" . 42053)
   ("read_to_output_reg_logic_rst" . 43499)
   ("bram_pointer_position_calc" . 44216)
   ("bram_pointer_position_rst" . 44699)
   ("load_output_reg" . 45008)
   ("load_output_reg_rst" . 46117))
  ("Processes"
   ("axi_bram_logic_l" . 50816)
   ("axi_bram_logic_r" . 51702)
   ("read_to_output_reg_l" . 52588)
   ("read_to_output_reg_r" . 53998)
   ("bram_ptr_pos_l_proc" . 55408)
   ("bram_ptr_pos_r_proc" . 56130)
   ("output_reg_loading_l" . 56853)
   ("output_reg_loading_r" . 58072)
   ("delay_output_reg_valid_l" . 59290)
   ("delay_output_reg_valid_r" . 59691))
  ("Instances"
   ("blk_mem_gen_0" . 48671)
   ("blk_mem_gen_0" . 49129)))
 ("core_fsm" . 60224)
 ("RTL"
  ("*architecture_body*" . 61028)
  ("Processes"
   ("fsm_axi_full" . 61560)))
 ("core_converter" . 64758)
 ("RTL"
  ("*architecture_body*" . 69266)
  ("Functions"
   ("clogb2" . 69309))
  ("Processes"
   ("fsm_proc" . 76576)
   ("read_burst_size_calc_proc" . 82084)
   ("araddr_proc" . 83926)
   ("arvalid_proc" . 84531)
   ("arlen_proc" . 85478)
   ("rdata_counter_proc" . 85835)
   ("rdata_total_counter_proc" . 86311)
   ("r_done_proc" . 86955)
   ("write_burst_size_calc_proc" . 87409)
   ("awaddr_proc" . 89364)
   ("strobe_burst_proc" . 89968)
   ("awlen_proc" . 91134)
   ("awvalid_proc" . 91491)
   ("wdata_counter_proc" . 92441)
   ("wdata_total_counter_proc" . 92964)
   ("wlast_proc" . 94162)
   ("bready_proc" . 95408)
   ("request_edge_detection_proc" . 96265)
   ("pattern_cnt_proc" . 96676)))
 ("axi_lite_regs" . 98535)
 ("RTL"
  ("*architecture_body*" . 101110)
  ("Procedures"
   ("add_bit" . 105604))
  ("Processes"
   ("axi_awready_proc" . 106465)
   ("axi_awaddr_proc" . 107109)
   ("axi_wready_proc" . 107732)
   ("axi_bvalid_proc" . 108531)
   ("axi_arready_proc" . 109530)
   ("axi_rvalid_proc" . 110619)
   ("axi_rdata_proc" . 111278)
   ("address_decoding_proc" . 111717)
   ("read_write_regs_proc" . 113845)
   ("read_only_regs_proc" . 118982)
   ("soft_reset_out_proc" . 121332)
   ("mm2s_proc" . 122005)
   ("master_lite_proc" . 122656))
  ("control_reg_signals_proc"
   ("*process_statement*" . 120721)
   ("Procedures"
    ("add_out_sigH" . 120780))))
 ("axi_lite_master" . 123512)
 ("RTL"
  ("*architecture_body*" . 125536)
  ("Processes"
   ("awvalid_proc" . 127058)
   ("wvalid_proc" . 127554)
   ("awaddr_proc" . 128043)
   ("wdata_proc" . 128400)
   ("bready_proc" . 128915)
   ("arvalid_proc" . 129401)
   ("rready_proc" . 129901)
   ("araddr_proc" . 130387)
   ("do_read" . 130742)
   ("error_proc" . 131534))))
