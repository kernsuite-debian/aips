      SUBROUTINE GTNAME (ILUN, NAMSTR, IERR)
C-----------------------------------------------------------------------
C! WAWA IO routine to fill in a namestring for an open file
C# IO-WaWa
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
C  GTNAME (for use with WAWA IO only) will fill in a name string for
C  a file open under a given logical unit number.
C  INPUTS:  ILUN    I    logical unit number of an open file.
C  OUTPUTS: NAMSTR  C*36 standard name string for the file.
C           IERR    I    standard WAWA IO error code.
C-----------------------------------------------------------------------
      CHARACTER NAMSTR*36, STAT*4, NAME*12, CLASS*6, PTYPE*2
      INTEGER   IBLK(256)
      INTEGER   ISEQ, IPTR, IERR, ILUN, ISLOT, IUSER, IVOL
      INCLUDE 'INCS:DITB.INC'
C-----------------------------------------------------------------------
C                                       Find LUN.
      DO 100 IPTR = 1,EFIL
         IF (FILTAB(POLUN,IPTR).EQ.ILUN) GO TO 200
 100     CONTINUE
C                                       File not open.
      IERR = 1
      GO TO 999
C                                       Get cat slot & disk volume.
 200  CONTINUE
      ISLOT = FILTAB(POCAT,IPTR)
      IVOL = FILTAB(POVOL,IPTR)
C                                       Fill in rest of name.
      CALL CATDIR (INFO, IVOL, ISLOT, NAME, CLASS, ISEQ,
     *   PTYPE, IUSER, STAT, IBLK, IERR)
      CALL A2WAWA (NAME, CLASS, ISEQ, PTYPE, IVOL, IUSER, NAMSTR)
      IF (IERR.NE.0) IERR = 10
C
 999  RETURN
      END
