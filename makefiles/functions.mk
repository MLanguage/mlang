##################################
#        Fonctions utiles        #
##################################

$(info Functions)

define eq
$(if $(or $(1),$(2)),$(and $(findstring $(1),$(2)),$(findstring $(2),$(1))),1)
endef

define not
$(if $(1),,1)
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

