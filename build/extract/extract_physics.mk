##############################################################################
# (c) Crown copyright 2024 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
#
# Run this file to extract source code from the UM repository.
#
# The following environment variables are used for input:
#   UM_FCM_TARGET_PLATFORM : Target identifier used to get the
#                            correct UM build configs.
#   PROFILE : Build profile used to determine optimisation level.
#   PROJECT_DIR : Full path to the current project's root directory.
#   SCRATCH_DIR : Temporary space for extracted source.
#   WORKING_DIR : Directory to hold working copies of source.
#
###############################################################################

.PHONY: extract
extract:
	# Retrieve and preprocess the UM, Jules and Socrates code
	# The UM_ENV file contains the appropriate locations and UM side
	# environment variables
	$Q. $(APPS_ROOT_DIR)/dependencies.sh \
	   && fcm make -C $(SCRATCH_DIR) -f $(APPS_ROOT_DIR)/build/extract/extract.cfg
	# Note that if wanting to modify UM source this should be done via the
	# UM repository either through a working copy or branch
	$Qrsync -acvz $(SCRATCH_DIR)/extract/ $(WORKING_DIR)/science/
