      SUBROUTINE ISTAB (ETYPE, IVOL, ISLOT, IVER, LUN, BUFFER, TABLE,
     *   EXIST, FITASC, IERR)
C-----------------------------------------------------------------------
C! finds if an extension file exists and whether it is a standard table
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2003
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
C   Given an extension type, volume, version, determine if the extension
C   file exists, and if so, is it a standard table.
C   Inputs:
C      ETYPE    C*2   Extension type like 'CC'
C      IVOL     I     Disk Volume.
C      ISLOT    I     Map catalog slot number.
C      LUN      I     AIPS LUN to use for opening & reading a block.
C   Output:
C      TABLE    L     True if this extension type is a table or this
C                     version does not exist.
C      EXIST    L     True if the extension file exists, else false.
C      FITASC   L     True if the file can be written as a FITS ASCII
C                     table.
C      IERR     I     Error code, 0=>OK else failed
C-----------------------------------------------------------------------
      CHARACTER ETYPE*2
      INTEGER   IVOL, ISLOT, IVER, LUN, BUFFER(256), IERR
      LOGICAL   TABLE, EXIST, FITASC
C
      CHARACTER PNAME*48, ATLAB*12, PATLAB*12
      LOGICAL   NOEXCL, NOWAIT, NOMAP
      INTEGER   FIND, IER, ISIZE
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (NOMAP, NOEXCL, NOWAIT)
      DATA NOMAP /.FALSE./
      DATA ATLAB /'*AIPS TABLE*'/
C-----------------------------------------------------------------------
C                                       Set up some fall thru values.
      TABLE = .TRUE.
      EXIST = .FALSE.
      FITASC = .FALSE.
C                                       Try to open file.
C                                       suppress file missing error
      CALL ZPHFIL (ETYPE, IVOL, ISLOT, IVER, PNAME, IERR)
      CALL ZEXIST (IVOL, PNAME, ISIZE, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.1) IERR = 0
         GO TO 999
         END IF
      CALL ZOPEN (LUN, FIND, IVOL, PNAME, NOMAP, NOEXCL, NOWAIT, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.2) IERR = 0
         GO TO 999
         END IF
      EXIST = .TRUE.
C                                       Read first block.
      CALL ZFIO ('READ', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Check that really a table file
      CALL H2CHR (12, 1, BUFFER(54), PATLAB)
      TABLE = PATLAB.EQ.ATLAB
C                                       Check if can be FITS ASCII
      FITASC = BUFFER(60).NE.1
C
 980  CALL ZCLOSE (LUN, FIND, IER)
      IF (IERR.EQ.0) IERR = IER
C
 999  RETURN
      END
