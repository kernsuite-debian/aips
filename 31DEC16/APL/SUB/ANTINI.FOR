      SUBROUTINE ANTINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
C-----------------------------------------------------------------------
C! creates and initializes antenna tables
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000-2001, 2005-2006, 2008-2009, 2015
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
C   Creates and initializes antenna tables: front routine for ANREFM
C   followed by ANINI.  This is the routine to call to open antenna
C   extension tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                           'WRIT' = create/init for write or read
C                           'READ' = open for read only
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      CATBLK   I(*)     Catalog header block.
C      LUN      I        Logical unit number to use
C   Input (create only) / output (pre-existing file):
C      VER      I        AN file version, actual value set on return
C      ARRAYC   D(3)     Array center X coord. (meters, earth center)
C      GSTIA0   D        GST at time=0 (degrees) on ref. date
C      DEGPDY   D        Earth rotation rate (deg/IAT day)
C      SAFREQ   D        Obs. Reference Frequency for subarray(Hz)
C      RDATE    C*8      Reference date as 'DD/MM/YY'
C      POLRXY   R(2)     Polar position X,Y (src sec) on ref. date
C      UT1UTC   R        UT1-UTC  (time sec.)          "
C      DATUTC   R        data time-UTC  (time sec.)          "
C      TIMSYS   C*8      Time system, 'IAT' or 'UTC'
C      ANAME    C*8      Array name
C      XYZHAN   C*8      Array handedness ('RIGHT' will be forced)
C      TFRAME   C*8      Terrestrial coordinate frame
C      NUMORB   I        Number of orbital parameters
C      NOPCAL   I        Number of polarization calibration constants.
C      ANTNIF   I        Number of IFs.
C      ANFQID   I        Table keyword, denotes the FQ ID for which
C                          the AN parms have been modified. On O/P if
C                          ANFQID = -999 it is not in the table, if
C                          ANFQID = -1 the virgin values still exist,
C                          or the data have no FREQID random parameter
C   Output:
C      IANRNO   I        Next scan number, start of the file if READ,
C                        the last+1 if WRITE
C      ANNUMV   I(*)     Element count in each column size MAXANC
C      ANKOLS   I(*)     The column pointer array in order, ANNAME,
C                        STABXYZ, ORBPARM, NOSTA, MNTSTA, STAXOF,
C                        POLTYA, POLAA, POLCALA, POLTYB, POLAB, POLCALB
C                        - size MAXANC
C      IERR     I        Return error code, 0=>OK, else error
C   Useage NOTE: use the include 'DANT.INC' for the declarations in
C   ANTINI and TABAN.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C                                       Any changes here should also be
C                                       made in DANT.INC and ANINI
      INTEGER   ANKOLS(MAXANC), ANNUMV(MAXANC), NUMORB, NOPCAL,
     *   IANRNO, ANTNIF, ANFQID
      CHARACTER ANAME*8, TIMSYS*8, RDATE*8, XYZHAN*8, TFRAME*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      DOUBLE PRECISION  ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IERR
C
      INTEGER   MVER, IVER
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL FNDEXT ('AN', CATBLK, MVER)
      IVER = VER
      IF (VER.LE.0) THEN
         IVER = MVER
         IF (OPCODE.EQ.'WRIT') IVER = MVER + 1
         END IF
C                                       known existing or READ
C                                       adjust handedness
C                                       add columns for diameter, FWHM
      IF ((OPCODE.EQ.'READ') .OR. (IVER.LE.MVER)) THEN
         CALL ANREFM (DISK, CNO, IVER, CATBLK, LUN, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C                                       now do its thing
      CALL ANINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN, IANRNO,
     *   ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL,
     *   ANTNIF, ANFQID, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTINI: ERROR',I3,' CORRECTING AN FORMAT, CONTINUING')
      END
