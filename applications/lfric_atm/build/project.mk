##############################################################################
# (c) Crown copyright 2017 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################

$(info UM physics project specials)

export PRE_PROCESS_INCLUDE_DIRS = \
        $(WORKING_DIR)/science/um/src/atmosphere/atmosphere_service/include \
        $(WORKING_DIR)/science/um/src/atmosphere/boundary_layer/include \
        $(WORKING_DIR)/science/um/src/atmosphere/large_scale_precipitation/include

export PRE_PROCESS_MACROS += UM_PHYSICS LFRIC USSPPREC_32B LSPREC_32B UM_JULES
