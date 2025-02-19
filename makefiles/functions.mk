##################################
#        Fonctions utiles        #
##################################

define to_bool
$(if $(1),1,)
endef

define not
$(if $(1),,1)
endef

define eq
$(call to_bool,$(if $(or $(1),$(2)),$(and $(findstring $(1),$(2)),$(findstring $(2),$(1))),1))
endef

define neq
$(call not,$(call eq,$(1),$(2)))
endef

define lt
$(shell [ $(1) -lt $(2) ] && echo "1")
endef

define le
$(shell [ $(1) -le $(2) ] && echo "1")
endef

define gt
$(shell [ $(1) -gt $(2) ] && echo "1")
endef

define ge
$(shell [ $(1) -ge $(2) ] && echo "1")
endef

define is_in
$(call eq,$(CURR_DIR)$(if $(1),,/),$(ROOT_DIR)/$(1))
endef

define make_in
$(if $(call not,$(call is_in,$(1))), \
  @$(MAKE) --no-print-directory -f $(ROOT_DIR)/Makefile -C $(ROOT_DIR)/$(1) ROOT_DIR=$(ROOT_DIR) $(2))
endef

define make_in_raw
$(MAKE) --no-print-directory -f $(ROOT_DIR)/Makefile -C $(ROOT_DIR)/$(1) ROOT_DIR=$(ROOT_DIR) $(2)
endef

define check_in
$(if $(call not,$(call is_in,$(1))), \
  $(error Rule $(2) forbidden in directory $(CURR_DIR), must be $$(ROOT_DIR)/$(1)))
endef

define source_dir
$(shell find $(1) -name tgvI.m) $(shell find $(1) -name errI.m) $(shell find $(1) -name \*.m ! -name err\*.m ! -name tgv\*.m | sort)
endef

define source_dir_ext
$(shell find $(1) -name \*.m | sort)
endef

define cmd_exists
$(shell command -v $(1) 2>&1 > /dev/null ] && echo "1")
endef

