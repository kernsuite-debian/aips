      SUBROUTINE SYMBOL (NTYPE)
C-----------------------------------------------------------------------
C! obtains symbol identification from symbol table; creates new symbols
C# POPS-lang
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
C-----------------------------------------------------------------------
C   SYMBOL obtains a symbol from the symbol list.  If not found and
C   MODE = 69 stores new symbol in linked list.  as coded, this routine
C   IS CASE SENSITIVE.  If the search is to be for only upper case,
C   the symbol must be converted (CHLTOU) prior to calling SYMBOL.
C   Inputs:
C      NTYPE   I     type to be assigned to new symbol
C   Commons in:
C      /SMSTUF/ KPAK  NKAR Hollerith characters to be found
C   Commons out:
C      /SMSTUF/ sets SYTYPE, LOCSYM, TAG
C      /POPS/ sets L to point at symbol area
C      /CORE/ sets new symbol into K as needed
C-----------------------------------------------------------------------
      INTEGER  NTYPE
C
      CHARACTER PRGNAM*6
      INTEGER   KXLINK, LENGTH, MK, N, NT, LLOCAT, RLOCAT, POTERR
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      DATA PRGNAM /'SYMBOL'/
C-----------------------------------------------------------------------
C                                       Search symbol table.
      L = K(1)
      NT = NKAR
      CALL CHUNT (K, L, NT, 4, KPAK)
      IF (IDEBUG.LE.0) GO TO 10
         WRITE (MSGTXT,1000) MODE, L, NT, NTYPE
         CALL MSGWRT (4)
C                                       Old symbol found.
 10   IF (L.GT.0) THEN
         LENGTH = K(L+1) / 16
         SYTYPE = K(L+1) - 16*LENGTH
         IF ((SYTYPE.NE.4) .AND. (SYTYPE.NE.5)) LOCSYM = L
         TAG = K(L+2)
C                                       New symbol - create new entry.
      ELSE
         POTERR = 2
         IF ((MODE.NE.69) .OR. (NTYPE.EQ.0)) GO TO 980
         SYTYPE = NTYPE
         N = (NT+3) / 4 + 4
         L = LLOCAT(N, K, K(9))
         IF (ERRNUM.NE.0) GO TO 980
         K(L+1) = 16*NT + SYTYPE
         LOCSYM = L
         MK = L + 4
         NT = (NT+3) / 4
         CALL RCOPY (NT, KPAK, CH(MK))
C                                       Point at verb, pseudoverb
         IF ((SYTYPE.EQ.4) .OR. (SYTYPE.EQ.5)) THEN
            TAG = NEWCOD
C                                       Allocate variable storage.
         ELSE
            KXLINK = 1
            TAG = RLOCAT (1, K(KXORG), C, KXLINK)
            IF (ERRNUM.NE.0) GO TO 980
            END IF
         K(L+2) = TAG
         END IF
      GO TO 999
C                                       Error
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SYMBOL: MODE,L,NT,NTYPE ',4I6)
      END
