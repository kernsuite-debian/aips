C   selection/calibration/editing class: name = 'CALEDIT'
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "selection/calibration/editing" class
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004, 2010, 2012
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
C   The parameters controling data selection, calibration and editing
C   are stored here.
C
C   Class public members.
C      SOURCS   C(30)*16  Names of up to 30 sources, *=>all
C                         First character of name '-' => all except
C                         those specified.
C      SELQUA   I         Qualifier wanted (-1 => all)
C      SELCOD   C*4       Cal code ('    ')
C      TIMRNG   R(8)      Start day, hour, min, sec, end day, hour,
C                         min, sec. 0's => all
C      UVRNG    R(2)      Minimum and maximum baseline lengths in
C                         1000's wavelengths. 0's => all
C      STOKES   C*4       Stokes types wanted.
C                         'I','Q','U','V','R','L','IQU','IQUV'
C                         '    '=> Leave data in same form as in input.
C      BCHAN    I         First channel number selected, 1 rel. to first
C                         channel in data base. 0 => all
C      ECHAN    I         Last channel selected. 0=>all
C      BIF      I         First IF number selected, 1 rel. to first
C                         IF in data base. 0 => all
C      EIF      I         Last IF selected. 0=>all
C      DOCAL    L         If true apply calibration, else not.
C      DOPOL    I         If >0 then correct for feed polarization
C                         based on antenna file info.
C      PDVER    I         PD table version for DOPOL>0
C      DOACOR   L         True if autocorrelations wanted (false)
C      DOXCOR   L         True if cross-correlations wanted (true)
C      DOWTCL   L         True if weight calibration wanted.
C      DOFQSL   L         True if FREQSEL random parm present (false)
C      FRQSEL   I         Default FQ table entry to select (-1)
C      SELBAN   R         Bandwidth (Hz) to select (-1.0)
C      SELFRQ   R         Frequency (Hz) to select (-1.0)
C      DOBAND   I         >0 if bandpass calibration. (-1)
C      DOSMTH   I         if smoothing requested, 1 before 2 after BP
C      SMOOTH   R(3)      Smoothing parameters (0.0s)
C      DXTIME   R         Integration time (days). Used when applying
C                         delay corrections to correct for delay error.
C      ANTENS   I(50)     List of antennas selected, 0=>all,
C                         any negative => all except those specified
C      SUBARR   I         Subarray desired, 0=>all
C      FGVER    I         FLAG file version number, if < 0 then
C                         NO flagging is applied. 0 => use highest
C                         numbered table.
C      CLUSE    I         Cal (CL or SN) file version number to apply.
C      BLVER    I         BL Table to apply .le. 0 => none
C      BPVER    I         BP table to apply .le. 0 => none
C
C   Public functions:
C      SECGET (name, keywrd, type, dim, value, valuec, ierr)
C         Return member.
C      SECPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Store  member.
C      SECINI (name, ierr)
C         Initializes default values (mostly no sel. edit, cal.)
C      SECSLT (name, bif, eif, bchan, echan, stokes, ierr)
C         Returns IF, channel and Stokes selection.
C      SECSAV (name, bif, eif, bchan, echan, stokes, ierr)
C         Resets IF, channel and Stokes selection.
C   Private to UV data class:
C      SECSET (name, ierr)
C         Copy current sel/edt/cal criteria to common
C
LOCAL INCLUDE 'CALEDIT.INC'
C                                       CALEDIT class include
      INTEGER   NMEML
      PARAMETER (NMEML = 30)
      CHARACTER MEMS(NMEML)*8, THSCLS*16
C                   1         2         3         4         5
      DATA MEMS /'SOURCS', 'SELQUA', 'SELCOD', 'TIMRNG', 'UVRNG',
C           6         7        8      9     10       11      12
     *   'STOKES', 'BCHAN', 'ECHAN', 'BIF', 'EIF', 'DOCAL', 'DOPOL',
C           13       14        15         16        17       18
     *   'DOACOR', 'DOXCOR', 'DOWTCL', 'DOFQSL', 'FRQSEL', 'SELBAN',
C           19        20        21        22         23        24
     *   'SELFRQ', 'DOBAND', 'DOSMTH' ,'SMOOTH', 'DXTIME', 'ANTENNS',
C           25      26        27       28       29       30
     *   'SUBARR', 'CLUSE', 'BLVER', 'BPVER', 'FGVER', 'PDVER'/
      DATA THSCLS /'CALEDIT'/
LOCAL END
      SUBROUTINE SECGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Return selection/calibration/editing member.
C   Inputs:
C      NAME    C*?    Object name
C      KEYWRD  C*(*)  Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
C
      INTEGER   IMEM, LOOP, OBJNUM, POINT
      CHARACTER MEMBER*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CALEDIT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       No base classes exist for this
C                                       class.
      IF (POINT.GE.1) THEN
         IERR = 2
         MSGTXT = 'NO BASE CLASSES FOR CLASS ' // THSCLS
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized
C                                       members.
      IMEM = -1
      DO 10 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       Find it?
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Fetch value:
      CALL OBGET (OBJNUM, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *   IERR)
C                                       Message if not found
      IF (IERR.EQ.1) THEN
         MSGTXT = 'MEMBER ' // MEMBER // ' NOT FOUND'
         CALL MSGWRT (6)
         MSGTXT = 'OBJECT =' // NAME
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
      END
      SUBROUTINE SECPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Store selection/calibration/editing member.
C   Inputs:
C      NAME    C*?    Object name
C      KEYWRD  C*(*)  Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
C
      INTEGER   IMEM, LOOP, OBJNUM, POINT
      CHARACTER MEMBER*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CALEDIT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       No base classes exist for this
C                                       class.
      IF (POINT.GE.1) THEN
         IERR = 2
         MSGTXT = 'NO BASE CLASSES FOR CLASS ' // THSCLS
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized
C                                       members.
      IMEM = -1
      DO 10 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       Find it?
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Fetch value:
      CALL OBPUT (OBJNUM, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *   IERR)
C                                       Message if not stored
      IF (IERR.NE.0) THEN
         MSGTXT = 'MEMBER ' // MEMBER // ' COULD NOT BE STORED'
         CALL MSGWRT (6)
         MSGTXT = 'OBJECT =' // NAME
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
      END
      SUBROUTINE SECINI (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Initialize selection/calibration/editing object.
C   Inputs:
C      NAME    C*?    Object name
C   Outputs:
C      IERR    I     Error code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAME*(*)
C
      INTEGER   DIM(3), OBJNUM, IVAL(50)
      LOGICAL   LVAL(50)
      REAL      RVAL(50)
      CHARACTER CVAL*480
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IVAL, RVAL, LVAL)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'CALEDIT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(3) = 0
C                                       Source list
      CVAL = '   '
      DIM(1) = 16
      DIM(2) = 30
      CALL OBPUT (OBJNUM, MEMS(1), OOACAR, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SELQUA
      IVAL(1) = -1
      DIM(1) = 1
      DIM(2) = 1
      CALL OBPUT (OBJNUM, MEMS(2), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SELCOD
      CVAL(1:4) = '    '
      DIM(1) = 4
      DIM(2) = 1
      CALL OBPUT (OBJNUM, MEMS(3), OOACAR, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Timerange
      CALL RFILL (8, 0.0, RVAL)
      DIM(1) = 8
      DIM(2) = 1
      CALL OBPUT (OBJNUM, MEMS(4), OOARE, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       UV range
      CALL RFILL (2, 0.0, RVAL)
      DIM(1) = 2
      DIM(2) = 1
      CALL OBPUT (OBJNUM, MEMS(5), OOARE, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       STOKES
      CVAL(1:4) = '    '
      DIM(1) = 4
      DIM(2) = 1
      CALL OBPUT (OBJNUM, MEMS(6), OOACAR, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BCHAN
      IVAL(1) = 1
      DIM(1) = 1
      DIM(2) = 1
      CALL OBPUT (OBJNUM, MEMS(7), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ECHAN
      IVAL(1) = 0
      CALL OBPUT (OBJNUM, MEMS(8), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BIF
      IVAL(1) = 1
      CALL OBPUT (OBJNUM, MEMS(9), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       EIF
      IVAL(1) = 0
      CALL OBPUT (OBJNUM, MEMS(10), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOCAL
      LVAL(1) = .FALSE.
      CALL OBPUT (OBJNUM, MEMS(11), OOALOG, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOPOL
      IVAL(1) = -1
      CALL OBPUT (OBJNUM, MEMS(12), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOACOR
      LVAL(1) = .FALSE.
      CALL OBPUT (OBJNUM, MEMS(13), OOALOG, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOXCOR (true)
      LVAL(1) = .TRUE.
      CALL OBPUT (OBJNUM, MEMS(14), OOALOG, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOWTCL
      LVAL(1) = .FALSE.
      CALL OBPUT (OBJNUM, MEMS(15), OOALOG, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOFQSL
      LVAL(1) = .FALSE.
      CALL OBPUT (OBJNUM, MEMS(16), OOALOG, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       FRQSEL
      IVAL(1) = -1
      CALL OBPUT (OBJNUM, MEMS(17), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SELBAN
      RVAL(1) = -1.0
      CALL OBPUT (OBJNUM, MEMS(18), OOARE, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SELFRQ (can only be passed as
C                                       real)
      RVAL(1) = -1.0
      CALL OBPUT (OBJNUM, MEMS(19), OOARE, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOBAND
      IVAL(1) = -1
      CALL OBPUT (OBJNUM, MEMS(20), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOSMTH
      IVAL(1) = 0
      CALL OBPUT (OBJNUM, MEMS(21), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SMOOTH
      CALL RFILL (3, 0.0, RVAL)
      DIM(1) = 3
      CALL OBPUT (OBJNUM, MEMS(22), OOARE, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DXTIME
      RVAL(1) = 0.0
      DIM(1) = 1
      CALL OBPUT (OBJNUM, MEMS(23), OOARE, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ANTENNAS
      CALL FILL (50, 0, IVAL)
      DIM(1) = 50
      CALL OBPUT (OBJNUM, MEMS(24), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SUBARR
      IVAL(1) = 0
      DIM(1) = 1
      CALL OBPUT (OBJNUM, MEMS(25), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CLUSE
      IVAL(1) = 0
      DIM(1) = 1
      CALL OBPUT (OBJNUM, MEMS(26), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BLVER
      IVAL(1) = -1
      DIM(1) = 1
      CALL OBPUT (OBJNUM, MEMS(27), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BPVER
      IVAL(1) = -1
      DIM(1) = 1
      CALL OBPUT (OBJNUM, MEMS(28), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       FGVER (no flagging)
      IVAL(1) = -100
      DIM(1) = 1
      CALL OBPUT (OBJNUM, MEMS(29), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       PDVER
      IVAL(1) = -1
      CALL OBPUT (OBJNUM, MEMS(30), OOAINT, DIM, IVAL, CVAL, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SECSET (NAME, IERR)
C-----------------------------------------------------------------------
C   Shared with UV data class
C   Does a variety of startup operations for establishing
C   selection/calibration and editing.  First SELINI is called to
C   initialize DSEL.INC.  Then the sel/cal/edit criteria are copied from
C   the object to DSEL.INC.  Finally, FQMATC is called to set selection
C   by FQ id.
C   Note: as POPS adverbs cannot be passed as double precision they are
C   stored as real.  It is intended that most of these values be filled
C   in by copying the adverb values from the Input object to the uv data
C   object.
C   Inputs:
C      NAME    C*?    Object name
C   Outputs:
C      IERR    I     Error code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAME*(*)
C
      INTEGER   TYPE, DIM(3), OBJNUM, LUN, DISK, CNO, DUMMY
      CHARACTER CDUMMY*1
      LOGICAL   MATCH
      REAL      RTEMP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'CALEDIT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Initialize DSEL.INC
      CALL SELINI
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Source list
      CALL OBGET (OBJNUM, MEMS(1), TYPE, DIM, DUMMY, SOURCS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SELQUA
      CALL OBGET (OBJNUM, MEMS(2), TYPE, DIM, SELQUA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SELCOD
      CALL OBGET (OBJNUM, MEMS(3), TYPE, DIM, DUMMY, SELCOD, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Timerange
      CALL OBGET (OBJNUM, MEMS(4), TYPE, DIM, TIMRNG, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       UV range
      CALL OBGET (OBJNUM, MEMS(5), TYPE, DIM, UVRNG, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       STOKES
      CALL OBGET (OBJNUM, MEMS(6), TYPE, DIM, DUMMY, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BCHAN
      CALL OBGET (OBJNUM, MEMS(7), TYPE, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ECHAN
      CALL OBGET (OBJNUM, MEMS(8), TYPE, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BIF
      CALL OBGET (OBJNUM, MEMS(9), TYPE, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       EIF
      CALL OBGET (OBJNUM, MEMS(10), TYPE, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOCAL
      CALL OBGET (OBJNUM, MEMS(11), TYPE, DIM, DOCAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOPOL
      CALL OBGET (OBJNUM, MEMS(12), TYPE, DIM, DOPOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOACOR
      CALL OBGET (OBJNUM, MEMS(13), TYPE, DIM, DOACOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOXCOR
      CALL OBGET (OBJNUM, MEMS(14), TYPE, DIM, DOXCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOWTCL
      CALL OBGET (OBJNUM, MEMS(15), TYPE, DIM, DOWTCL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOFQSL
      CALL OBGET (OBJNUM, MEMS(16), TYPE, DIM, DOFQSL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       FRQSEL
      CALL OBGET (OBJNUM, MEMS(17), TYPE, DIM, FRQSEL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SELBAN
      CALL OBGET (OBJNUM, MEMS(18), TYPE, DIM, SELBAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SELFRQ (can only be passed as
C                                       real)
      CALL OBGET (OBJNUM, MEMS(19), TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      SELFRQ = RTEMP
C                                       DOBAND
      CALL OBGET (OBJNUM, MEMS(20), TYPE, DIM, DOBAND, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOSMTH
      CALL OBGET (OBJNUM, MEMS(21), TYPE, DIM, DOSMTH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SMOOTH
      CALL OBGET (OBJNUM, MEMS(22), TYPE, DIM, SMOOTH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DXTIME
      CALL OBGET (OBJNUM, MEMS(23), TYPE, DIM, DXTIME, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ANTENNAS
      CALL OBGET (OBJNUM, MEMS(24), TYPE, DIM, ANTENS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SUBARR
      CALL OBGET (OBJNUM, MEMS(25), TYPE, DIM, SUBARR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CLUSE
      CALL OBGET (OBJNUM, MEMS(26), TYPE, DIM, CLUSE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BLVER
      CALL OBGET (OBJNUM, MEMS(27), TYPE, DIM, BLVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BPVER
      CALL OBGET (OBJNUM, MEMS(28), TYPE, DIM, BPVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       FGVER
      CALL OBGET (OBJNUM, MEMS(29), TYPE, DIM, FGVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       PDVER
      CALL OBGET (OBJNUM, MEMS(30), TYPE, DIM, PDVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Process FQ selection criteria.
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBDSKC (NAME, DISK, CNO, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBHGET (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Need to update DUVH.INC
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FQMATC (DISK, CNO, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IERR)
C                                       Free LUN
      CALL OBLUFR (LUN)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         CALL MSGWRT (8)
         IERR = 1
      ELSE
         IERR = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE SECSLT (NAME, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
C-----------------------------------------------------------------------
C   Public
C   Return current IF, channel and Stokes' selection criteria.
C   Inputs:
C      NAME    C*?    Object name
C   Outputs:
C      BIF     I     First IF selected
C      EIF     I     Highest IF selected
C      BCHAN   I     First channel selected
C      ECHAN   I     Highest channel selected
C      STOKES  C*4   Stokes selection string
C      IERR    I     Error code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   BIF, EIF, BCHAN, ECHAN, IERR
      CHARACTER NAME*(*), STOKES*4
C
      INTEGER   TYPE, DIM(3), OBJNUM, DUMMY
      CHARACTER CDUMMY*1
      INCLUDE 'CALEDIT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look up values
      CALL OBGET (OBJNUM, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJNUM, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJNUM, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJNUM, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJNUM, 'STOKES', TYPE, DIM, DUMMY, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SECSAV (NAME, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
C-----------------------------------------------------------------------
C   Public
C   Reset IF, channel and Stokes' selection criteria to specified
C   values.
C   Inputs:
C      BIF     I     First IF selected
C      EIF     I     Highest IF selected
C      BCHAN   I     First channel selected
C      ECHAN   I     Highest channel selected
C      STOKES  C*4   Stokes selection string
C      NAME    C*?    Object name
C   Outputs:
C      IERR    I     Error code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   BIF, EIF, BCHAN, ECHAN, IERR
      CHARACTER NAME*(*), STOKES*4
C
      INTEGER   DIM(3), OBJNUM, DUMMY
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'CALEDIT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save values
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OBPUT (OBJNUM, 'BIF', OOAINT, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJNUM, 'EIF', OOAINT, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJNUM, 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJNUM, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = LEN (STOKES)
      CALL OBPUT (OBJNUM, 'STOKES', OOACAR, DIM, DUMMY, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
