      SUBROUTINE CONCAT
C-----------------------------------------------------------------------
C! creates temporary literal on stack = concatanation of 2 strings
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C   CONCAT creates a temporary literal string (SYTYPE 14) with pointers
C   on the STACK containing the concatanated contents of 2 strings
C   which must be on the STACK on input.
C   alters K array; SP in /POPS/; SYTYPE and TAG in /SMSTUF/
C-----------------------------------------------------------------------
      INTEGER   POTERR, ICSIZE, ISIZE, IOFF, ITAG, I, IACT,
     *   ITYPE, NCSIZE, NSIZE, NTAG, NTYPE, NOFF, LSAVE, NACT
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGNAM /'CONCAT'/
C-----------------------------------------------------------------------
C                                       Check stack
      POTERR = 7
      IF (SP.LT.8) GO TO 980
      POTERR = 8
      IF ((STACK(SP).NE.2) .OR. (STACK(SP-4).NE.2)) GO TO 980
C                                       Pick up pointers
      ITAG = STACK(SP-1)
      ISIZE = STACK(SP-2)
      ITYPE = STACK(SP-3)
      NTAG = STACK(SP-5)
      NSIZE = STACK(SP-6)
      NTYPE = STACK(SP-7)
      SP = SP - 4
C                                       Must be characters
      IF (((NTYPE.LT.7) .OR. (NTYPE.GT.9)) .AND. (NTYPE.NE.14))
     *   GO TO 980
      IF (((ITYPE.LT.7) .OR. (ITYPE.GT.9)) .AND. (ITYPE.NE.14))
     *   GO TO 980
      IF ((ITYPE.EQ.7) .AND. (K(ISIZE+1).GT.1)) GO TO 980
      IF ((NTYPE.EQ.7) .AND. (K(NSIZE+1).GT.1)) GO TO 980
C                                       Get offsets, numb characters
      IOFF = 1
      IF (ITYPE.EQ.9) IOFF = ISIZE / 1024
      ICSIZE = ISIZE
      IF (ITYPE.EQ.7) ICSIZE = K(ISIZE+3)
      IF (ITYPE.EQ.9) ICSIZE = MOD (ISIZE, 1024)
      NOFF = 1
      IF (NTYPE.EQ.9) NOFF = NSIZE / 1024
      NCSIZE = NSIZE
      IF (NTYPE.EQ.7) NCSIZE = K(NSIZE+3)
      IF (NTYPE.EQ.9) NCSIZE = MOD (NSIZE, 1024)
C                                       trailing blanks don't count
      CALL SPFIL (CH(NTAG), NCSIZE, NACT)
      NCSIZE = NACT - NOFF + 1
      IF (NCSIZE.LE.0) NCSIZE = 0
      CALL SPFIL (CH(ITAG), ICSIZE, IACT)
      ICSIZE = IACT - IOFF + 1
      IF (ICSIZE.LE.0) ICSIZE = 1
C                                       Make space
      LX = ICSIZE + NCSIZE
      POTERR = 13
      IF (LX.GT.132) GO TO 980
      SYTYPE = 14
      LX = (LX + 3) / 4
      IF (LX.LT.1) LX = 1
      LSAVE = L
      L = MIN (132, NCSIZE)
      IF (NCSIZE.GT.0) CALL CHCOPY (L, NOFF, CH(NTAG), 1, X)
      I = MIN (132-L, ICSIZE)
      L = L + 1
      IF (L.LE.133-I) CALL CHCOPY (I, IOFF, CH(ITAG), L, X)
      CALL LTSTOR
      L = LSAVE
      IF (ERRNUM.EQ.1) ERRNUM = 75
      IF (ERRNUM.NE.0) GO TO 980
      K(LOCSYM+1) = SYTYPE + 16 * (ICSIZE + NCSIZE)
C                                       Move characters: not needed
c     CALL CHCOPY (NCSIZE, NOFF, CH(NTAG), 1, CH(TAG))
C     NOFF = NCSIZE + 1
C     CALL CHCOPY (ICSIZE, IOFF, CH(ITAG), NOFF, CH(TAG))
C                                       Put on stack
      STACK(SP-3) = 14
      STACK(SP-2) = NCSIZE + ICSIZE
      STACK(SP-1) = TAG
      GO TO 999
C                                        error return
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
