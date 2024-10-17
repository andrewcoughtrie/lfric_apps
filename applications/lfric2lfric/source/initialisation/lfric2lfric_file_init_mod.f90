!-------------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!> @brief   Sets up I/O configuration from within lfric2lfric.
!> @details Collects configuration information relevant for the I/O subsystem
!!          and formats it so that it can be passed to the infrastructure.
module lfric2lfric_file_init_mod

  use constants_mod,       only : i_def
  use driver_modeldb_mod,  only : modeldb_type
  use file_mod,            only : FILE_MODE_READ, &
                                  FILE_MODE_WRITE
  use files_config_mod,    only : checkpoint_stem_name, &
                                  diag_stem_name,       &
                                  start_dump_filename
  use io_config_mod,       only : diagnostic_frequency, &
                                  checkpoint_write,     &
                                  checkpoint_read,      &
                                  write_diag,           &
                                  use_xios_io
  use lfric_xios_file_mod, only : lfric_xios_file_type, &
                                  OPERATION_TIMESERIES
  use linked_list_mod,     only : linked_list_type

  implicit none

  private
  public :: init_lfric2lfric_src_files, init_lfric2lfric_dst_files

  contains

  !> @brief   Sets up source I/O configuration.
  !> @details Initialises the file list for the source I/O context, using the
  !!          start_dump_filename extracted from the `files` namelist.
  !> @param [out]        files_list    The list of I/O files.
  !> @param [in,out]     modeldb       Required by init_io.
  subroutine init_lfric2lfric_src_files( files_list, modeldb )

    implicit none

    type(linked_list_type),        intent(out)   :: files_list
    type(modeldb_type), optional,  intent(inout) :: modeldb

    if( use_xios_io ) then

      ! Set up diagnostic writing info
      if( write_diag ) then
        ! Setup diagnostic output file
        call files_list%insert_item(                         &
            lfric_xios_file_type( trim( diag_stem_name ),    &
                                  xios_id="lfric_diag",      &
                                  io_mode=FILE_MODE_WRITE,   &
                                  freq=diagnostic_frequency) )
      endif

      ! Setup checkpoint reading context information
      call files_list%insert_item(                               &
          lfric_xios_file_type( trim(start_dump_filename),       &
                                xios_id="lfric_checkpoint_read", &
                                io_mode=FILE_MODE_READ )         )

    endif

  end subroutine init_lfric2lfric_src_files

  !> @brief   Sets up destination I/O configuration.
  !> @details Initialises the file list for the destination I/O context, using
  !!          the checkpoint_stem_name extracted from the `files` namelist.
  !> @param [out]        files_list    The list of I/O files.
  !> @param [in,out]     modeldb       Required by init_io.
  subroutine init_lfric2lfric_dst_files( files_list, modeldb )

    implicit none

    type(linked_list_type),        intent(out)   :: files_list
    type(modeldb_type), optional,  intent(inout) :: modeldb

    ! Local variables
    integer(kind=i_def), parameter :: checkpoint_frequency = 1_i_def

    if( use_xios_io ) then

      ! Setup checkpoint reading context information
      if ( checkpoint_write ) then
          call files_list%insert_item(                                &
              lfric_xios_file_type( trim( checkpoint_stem_name ),     &
                                    xios_id="lfric_checkpoint_write", &
                                    io_mode=FILE_MODE_WRITE,          &
                                    operation=OPERATION_TIMESERIES,   &
                                    freq=checkpoint_frequency )       )
      endif

    endif

  end subroutine init_lfric2lfric_dst_files

end module lfric2lfric_file_init_mod
