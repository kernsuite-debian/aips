      PROGRAM HOK
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
C   Reads an AIPS address file and identifies sites which have not given
C   permission for their data to be published (\h0 [site-wide attribute
C   line] does not contain 'OK=Y').  Use with aipswho:whogets.adr,
C   sorted to eliminate NA (Not an active site), BA (Bad address), and
C   COM (Commercial site).
C   By Don Wells, 4/88, for the 1987 AIPS Site Survey
C   Fixed to look for 'OK=Y' not just 'OK' (because the ones who do not
C   give permission should say 'OK=N'), 13-May-89, NDW
C-----------------------------------------------------------------------
      CHARACTER LINE*100, FILE*60, LASTCP*40
      INTEGER K
C-----------------------------------------------------------------------
C     Initialize variables:
      K = 0
      LASTCP = ' '
C                                       Prompt for file name:
      WRITE (6,*) 'Enter filename (list-directed form, with quotes):'
      READ (5,*) FILE
      OPEN (1,FILE=FILE,STATUS='OLD')
 110  READ (1,'(A)',END=190) LINE
      K = K + 1
      IF ((LINE.EQ.' ').OR.(LASTCP.EQ.' ')) LASTCP = LINE
C                                       If not an \h0 line, read
C                                       the next line:
      IF (LINE(1:4).NE.'\h0 ') GO TO 110
C                                       If no 'OK=Y', write CP last
C                                       name and the \h0 line:
      IF (INDEX (LINE, 'OK=Y').EQ.0) WRITE(6,*)
     *      '[', LASTCP(1:20), '] LINE=[', LINE(1:50)
      GO TO 110
 190  STOP
      END
