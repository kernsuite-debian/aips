      SUBROUTINE ALSPUT (ALIST, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C! Store value to Associative list (ALIST)
C# Utility 
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
C   Public
C   Save an entry in an object creating it if necessary.
C   Recognizes special keyword 'LISTNAME' as the name of the ALIST.
C   See ALSINI for details of the package.
C   Note the AIPS datatypes are defined as parameters in the INCLUDE
C   'INCS:PAOOF.INC': OOADP = double precision, OOARE = real, 
C   OOACAR = character, OOAINT = integer, and OOALOG = logical.
C   Inputs:
C      ALIST  I(*)  ALIST array
C      KEYWRD  C*16  Keyword
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are undefined.
C      VALUE   ?     Associated value (non character)
C      VALUEC  C(*)*? Associated value (character)
C   Outputs:
C      IERR    I     Error code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   ALIST(*), TYPE, DIM(*), VALUE(*), IERR
      CHARACTER KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   NVAL, KEYPNT, IDIM(100), NDIM, DATOFF, LHEAD, I
      CHARACTER ALNAME*32
      INCLUDE 'INCS:PALIST.INC'
C-----------------------------------------------------------------------
      IERR = 0
      NDIM = ALIST(ALSNDM)
      DATOFF = ALIST(ALSDAT)
      LHEAD = ALIST(ALSHED)
C                                       Validity test
      IF (ALIST(ALSCHK).NE.ALSCVL) THEN
         MSGTXT = 'ALSPUT: DAMAGED OR UNINITIALIZED ALIST'
         IERR = 6
         GO TO 990
         END IF
C                                       Special trap for ALIST name
      IF (KEYWRD(1:8).EQ.'LISTNAME') THEN
         CALL CHR2H (32, VALUEC, 1, ALIST(ALSNAM))
         GO TO 999
         END IF
C                                       Size of value
      NVAL = 1
      DO 20 I = 1,NDIM
         IDIM(I) = MAX (1, DIM(I))
         NVAL = NVAL * IDIM(I)
 20      CONTINUE
C                                       DP may be longer
      IF (TYPE.EQ.1) NVAL = NVAL * NWDPDP
C                                       See if it's there?
      CALL ALSKEY (ALIST, KEYWRD, KEYPNT, IERR)
C                                       Does it currently exist?
      IF (IERR.EQ.0) THEN
C                                       Update existing entry.
C                                       Check type
         IF (TYPE.NE.ALIST(KEYPNT+ALSTYP)) THEN
            IERR = 1
            MSGTXT = 'WRONG KEYWORD DATA TYPE'
            GO TO 990
            END IF
C                                       Check dimensions
         DO 40 I = 1,NDIM
            IF (IDIM(I).NE.ALIST(KEYPNT+ALSDIM+I-1)) THEN
               IERR = 1
               MSGTXT = 'WRONG KEYWORD VALUE DIMENSIONALITY'
               GO TO 990
               END IF
 40         CONTINUE
C                                       Character
         IF (TYPE.EQ.3) THEN
            CALL ALSC2H (VALUEC, NDIM, IDIM, ALIST(KEYPNT+DATOFF))
C                                       Other
         ELSE
            CALL COPY (NVAL, VALUE, ALIST(KEYPNT+DATOFF))
            END IF
      ELSE
         IERR = 0
C                                       Add new keyword.
C                                       Check if there's room
         IF ((ALIST(ALSEND)+NVAL+LHEAD) .GT. ALIST(ALSIZE)) THEN
            IERR = 3
            MSGTXT = 'ALIST TOO SMALL TO ALLOCATE NEW KEYWORD'
            GO TO 990
            END IF
C                                       Pointer to next free word
         KEYPNT = ALIST(ALSEND)
         ALIST(ALSEND) = KEYPNT + LHEAD + NVAL
C                                       New list termination
         ALIST(ALIST(ALSEND)) = -1
C                                       Add Keyword
         CALL CHR2H (16, KEYWRD, 1, ALIST(KEYPNT+ALSKWD))
C                                       Pointer to next cell
         ALIST(KEYPNT+ALSNXT) = ALIST(ALSEND)
C                                       Type
         ALIST(KEYPNT+ALSTYP) = TYPE
C                                       Dimensions
         CALL COPY (NDIM, IDIM, ALIST(KEYPNT+ALSDIM))
C                                       Character
         IF (TYPE.EQ.3) THEN
            CALL ALSC2H (VALUEC, NDIM, IDIM, ALIST(KEYPNT+DATOFF))
C                                       Other
         ELSE
            CALL COPY (NVAL, VALUE, ALIST(KEYPNT+DATOFF))
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      MSGTXT = 'KEYWORD =' // KEYWRD
      CALL MSGWRT (6)
      CALL H2CHR (32, 1, ALIST(ALSNAM), ALNAME)
      MSGTXT = 'PROBLEM WITH ALIST: ' // ALNAME
      CALL MSGWRT (6)
C
 999  RETURN
      END
