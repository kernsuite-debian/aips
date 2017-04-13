      SUBROUTINE ALSGET (ALIST, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C! Fetch value from Associative list (ALIST)
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
C   Fetch the value (array) for a specified keyword.
C   Recognizes special keyword 'LISTNAME' as the name of the ALIST
C   Recognizes special keyword 'GET SIZE' as the size of the ALIST
C   See ALSINI for details of the package.
C   Note the AIPS datatypes are defined as parameters in the INCLUDE
C   'INCS:PAOOF.INC': OOADP = double precision, OOARE = real, 
C   OOACAR = character, OOAINT = integer, and OOALOG = logical.
C   Inputs:
C      ALIST  I(*)  ALIST array
C      KEYWRD C*16  Keyword
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C(*)*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   ALIST(*), TYPE, DIM(*), VALUE(*), IERR
      CHARACTER KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   NVAL, KEYPNT, NDIM, DATOFF, I
      CHARACTER ALNAME*32
      INCLUDE 'INCS:PALIST.INC'
C-----------------------------------------------------------------------
      IERR = 0
      NDIM = ALIST(ALSNDM)
      DATOFF = ALIST(ALSDAT)
C                                       Validity test
      IF (ALIST(ALSCHK).NE.ALSCVL) THEN
         MSGTXT = 'ALSGET: DAMAGED OR UNINITIALIZED ALIST'
         IERR = 6
         GO TO 990
         END IF
C                                       Special trap for ALIST name
      IF (KEYWRD(1:8).EQ.'LISTNAME') THEN
         CALL H2CHR (32, 1, ALIST(ALSNAM), VALUEC)
         TYPE = 3
         DIM(1) = 32
         CALL FILL (NDIM-1, 1, DIM(2))
         GO TO 999
C                                       Special trap for ALIST size
      ELSE IF (KEYWRD(1:8).EQ.'GET SIZE') THEN
         VALUE(1) = ALIST(ALSIZE)
         TYPE = 4
         CALL FILL (NDIM, 1, DIM(1))
         GO TO 999
         END IF
C                                       See if keyword is in the list
      CALL ALSKEY (ALIST, KEYWRD, KEYPNT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'COULD NOT FIND ALIST KEYWORD '
         GO TO 990
         END IF
C                                       Type
      TYPE = ALIST(KEYPNT+ALSTYP)
C                                       Dimensions
      CALL COPY (NDIM, ALIST(KEYPNT+ALSDIM), DIM)
C                                       Get data: Character
C                                       Size of value
      NVAL = 1
      DO 20 I = 1,NDIM
         NVAL = NVAL * DIM(I)
 20      CONTINUE
      IF (TYPE.EQ.1) NVAL = NVAL * NWDPDP
      IF (TYPE.EQ.3) THEN
C                                       Check String length
         IF (LEN (VALUEC(1)) .LT. DIM(1)) THEN
            IERR = 5
            MSGTXT = 'ALSGET: OUTPUT CHARACTER STRING TOO SMALL'
            GO TO 990
            END IF
         CALL ALSH2C (ALIST(KEYPNT+DATOFF), NDIM, DIM, VALUEC)
C                                       Other
      ELSE
         CALL COPY (NVAL, ALIST(KEYPNT+DATOFF), VALUE)
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
