      SUBROUTINE ZTXMAT (IVOL, PNAME, MNAME, NCH, VERSON, NAMES, NNAM,
     *   IERR)
C-----------------------------------------------------------------------
C! return list of files in specified area beginning with specified chars
C# Z Text
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
C   Open a directory and find a list of member file names whose first
C   NCH characters match the first NCH characters of MNAME plus
C   extension (based on file type).
C   Inputs:
C      IVOL     I         Disk volume containing file, (ignored)
C      PNAME    C*48      Physical file name (only used to determine
C                         file type)
C      MNAME    C*8       Test file name
C      NCH      I         Number of characters to compare (<= 8)
C                            0 is okay -> get full directory
C      VERSON   C*48      Logical name for directory or version of
C                            directory to search (for file type
C                            specific directories)
C   In/out:
C      NNAM     I         Number of names in NAME: input = max,
C                            output = actual used
C   Output:
C      NAMES    H*8(*)    File names which match the given file spec
C                         NOTE HOLLERITH for lower level routines
C      IERR     I         Error return code: 0 => no error
C                            1 => no matches found
C                            2 => error in inputs
C                            3 => error opening directory
C   UNICOS version - uses ZDIR, ZFULLN, ZTXMA2
C-----------------------------------------------------------------------
      INTEGER   IVOL, NCH, NNAM, IERR
      CHARACTER VERSON*48, PNAME*48, MNAME*8
      HOLLERITH NAMES(2,*)
C
      INTEGER   I, NMAX, IEXT, WLEN, FLEN, NNAM2, ISTART, ITRIM
      CHARACTER FILSPC*128, XLATED*128, WCNAME*9, TYPE*2
      CHARACTER*8 CNAMES(64)
C-----------------------------------------------------------------------
      NMAX = NNAM
C                                       Get file type (e.g., 'HE' for
C                                       HELP files).
      I = INDEX (PNAME, ':')
      TYPE = PNAME(I+1:I+2)
C                                       Form wild card file name
C                                       specification.
      IF (NCH.GT.0) THEN
         WCNAME = MNAME(1:NCH) // '*'
      ELSE
         WCNAME = '*'
         END IF
      WLEN = NCH + 1
C                                       Create complete file spec (ZDIR
C                                       will append proper extension)
      CALL ZDIR (TYPE, VERSON, WCNAME, FILSPC, IERR)
C                                       Find all the matches (IEXT = 0
C                                       means remove extensions).
      IF (IERR.EQ.0) THEN
         IEXT = 0
         FLEN = ITRIM (FILSPC)
         CALL ZTXMA2 (FLEN, FILSPC, NMAX, IEXT, NNAM, CNAMES, IERR)
C                                       RUN - concatanate list from
C                                       system area also
         IF ((IERR.LE.1) .AND. (TYPE.EQ.'RU')) THEN
            ISTART = NNAM + 1
            NMAX = NMAX - NNAM
            XLATED = 'RUNSYS:' //  WCNAME(1:WLEN) // '.001'
            CALL ZFULLN (XLATED, ' ', ' ', FILSPC, IERR)
C                                       Look in RUNSYS for a match.
            IF (IERR.EQ.0) THEN
               FLEN = ITRIM (FILSPC)
               CALL ZTXMA2 (FLEN, FILSPC, NMAX, IEXT, NNAM2,
     *            CNAMES(ISTART), IERR)
               NNAM = NNAM + NNAM2
               END IF
            END IF
         END IF
      DO 100 I = 1,NNAM
         CALL CHR2H (8, CNAMES(I), 1, NAMES(1,I))
 100     CONTINUE
C
 999  RETURN
      END
