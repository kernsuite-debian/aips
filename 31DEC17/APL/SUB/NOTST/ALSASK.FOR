      SUBROUTINE ALSASK (ALIST, KEYWRD, TYPE, DIM, IERR)
C-----------------------------------------------------------------------
C! Inquire about an entry in an Associative list (ALIST)
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
C   Inquire about an entry in an Associative list (ALIST)
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
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   ALIST(*), TYPE, DIM(*), IERR
      CHARACTER KEYWRD*(*)
C
      INTEGER   KEYPNT, NDIM, DATOFF
      CHARACTER ALNAME*32
      INCLUDE 'INCS:PALIST.INC'
C-----------------------------------------------------------------------
      IERR = 0
      NDIM = ALIST(ALSNDM)
      DATOFF = ALIST(ALSDAT)
C                                       Validity test
      IF (ALIST(ALSCHK).NE.ALSCVL) THEN
         MSGTXT = 'ALSASK: DAMAGED OR UNINITIALIZED ALIST'
         IERR = 6
         GO TO 990
         END IF
C                                       See if keyword is in the list
      CALL ALSKEY (ALIST, KEYWRD, KEYPNT, IERR)
      IF (IERR.NE.0) THEN
C                                       Didn't find
         IERR = 1
         GO TO 999
         END IF
C                                       Type
      TYPE = ALIST(KEYPNT+ALSTYP)
C                                       Dimensions
      CALL COPY (NDIM, ALIST(KEYPNT+ALSDIM), DIM)
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
