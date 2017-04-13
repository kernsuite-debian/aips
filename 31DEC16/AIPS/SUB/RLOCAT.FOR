      INTEGER FUNCTION RLOCAT (NWORDS, ARRAY, DATAR, LPT)
C-----------------------------------------------------------------------
C! allocates space in linked-list array and handles link pointers
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
C   RLOCAT allocates NWORDS floating point locations from the array
C   DATAR and links the last allocated block to the current block
C   by the pointer LPT in the pointer array ARRAY.
C   Inputs:
C      NWORDS  I      # words of storage needed
C   In/out
C      ARRAY   I(*)   in:  (3) next free space in DATAR
C                     out: (3) new next free space in DATAR
C                     in:  (5) max available space in DATAR
C                     out: (in LPT) linked to new allocated space
C      LPT     I      in:  place to link last old structure to new
C                     out: point to allocated space
C   Outputs:
C      DATAR   R(*)   (LPTout:LPTout+NWORDS-1) set to zero
C      RLOCAT  I      point to allocated space
C-----------------------------------------------------------------------
      REAL      DATAR(*)
      INTEGER   NWORDS, ARRAY(*), LPT
C
      INTEGER   I, J, N, NFC
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      DATA PRGNAM /'RLOCAT'/
C-----------------------------------------------------------------------
C                                        NFC = point to next free space
      NFC = ARRAY(3)
C                                        N = max available space
      N = ARRAY(5)
      J = N - NFC - NWORDS + 1
      IF (J.GT.0) THEN
         RLOCAT = NFC
C                                        link to new space
         ARRAY(LPT) = NFC
         NFC = NFC + NWORDS
C                                        return pointer to new
         LPT = RLOCAT
C                                        clear new space
         J = NFC - 1
         DO 50 I = LPT,J
            DATAR(I) = 0.
 50         CONTINUE
         ARRAY(3) = NFC
C                                        Error: no room
      ELSE
         RLOCAT = -1
         ERRNUM = 1
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
      END
