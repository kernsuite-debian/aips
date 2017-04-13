      SUBROUTINE ZDCHIC (NBITWD, NBITCH, NCHLIN, NWDPDP, BYTFLP,
     *   SPFRMT, DPFRMT, NBPS, NMFCB, MFCB, NFCBER, NFCBFD, FCBFD,
     *   FCBREQ, FCBERR, FCBXFR, FCBSIZ, FCBSP1, FCBSP2, FCBSP3, FCBSP4,
     *   SYSTYP, SYSVER)
C-----------------------------------------------------------------------
C! set more system parameters; make them available to C routines
C# Z2 System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Make various device characteristics as defined in ZDCHIN available
C   to routines written in C (e.g., extern int Z_bytflp).
C   Inputs:
C      NBITCH   I   # bits per character
C      NCHLIN   I   # characters per input line
C      BYTFLP   I   Byte flip indicator code
C      SPFRMT   I   Single precision floating-point format code
C      DPFRMT   I   Double precision floating-point format code
C      NBPS     I   # bytes (i.e., AIPS-bytes) per disk sector
C                      (assigned to Z_nbps as # of 8-bit bytes)
C      NMFCB    I   # elements in FTAB for non-map file control blocks
C      MFCB     I   # elements in FTAB for map file control blocks
C      NFCBER   I   Offset in non-file control blocks to error code
C      NFCBFD   I   Offset in non-file control blocks for file descriptor
C      FCBFD    I   Offset in file control blocks to file descriptor
C      FCBREQ   I   Offset in file control blocks to the I/O request
C      FCBERR   I   Offset in file control blocks to system error code
C      FCBXFR   I   Offset in file control block to I/O transfer count
C      FCBSIZ   I   Offset in file control blocks to the file size
C      FCBSP1   I   Offset in file control blocks to the 1st spare
C      FCBSP2   I   Offset in file control blocks to the 2nd spare
C      FCBSP3   I   Offset in file control blocks to the 3rd spare
C      FCBSP4   I   Offset in file control blocks to the 4th spare
C      SYSVER   C*8 system version: '4.5', 'BSD 4.2', 'SYS 5', ...
C      SYSTYP   C*4 system type: 'VMS ', 'SUN3', 'SUN4', 'DEC ', 'ALPH',
C                   'IBM', 'CVEX', ...
C   In/Out:
C      NBITWD   I   # bits per word
C      NWDPDP   I   # words per double precision floating-point
C   Generic version - non-error STUB.
C-----------------------------------------------------------------------
      INTEGER   NBITWD, NBITCH, NCHLIN, NWDPDP, BYTFLP, SPFRMT, DPFRMT,
     *   NBPS, NMFCB, MFCB, NFCBER, NFCBFD, FCBFD, FCBREQ, FCBERR,
     *   FCBXFR, FCBSIZ, FCBSP1, FCBSP2, FCBSP3, FCBSP4
      CHARACTER SYSTYP*(*), SYSVER*(*)
C
C-----------------------------------------------------------------------
C                                       Do nothing here
 999  RETURN
      END
