      SUBROUTINE OPEXT (TYPE, IVOL, ISLOT, IVER, ILUN, EXCL, WAIT,
     *   IFIND, IERR)
C-----------------------------------------------------------------------
C! opens a specified extension file
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   OPEXT willl open a particular extension file.  If IVER is zero the
C   map file header willl be searched for the latest version no.  The
C   map header and parameters must be in common and the subroutine
C   VHDRIN must have been run.
C   Inputs:
C      TYPE   C*2  the extension file type (ie 'PL', 'SL', ect.).
C      IVOL   I    the disk volume no. of the extension file.
C      ISLOT  I    the catalog slot number of the map file.
C      ILUN   I    the logical unit number to use for the file.
C      EXCL   L    exclusive use flag.
C      WAIT   L    wait until available flag.
C   In/out:
C      IVER   I    the version number of the extension file ( 0
C                  means use latest in map header.) Returns used.
C   Outputs:
C      IFIND  I    the index to FTAB information.
C      IERR   I    error indicator. 0 = o.k.
C                     1 = could not build file name.
C                     2 = no extension file in map header.
C                     3 = could not open extension file: some error
C                     4 = could not open extension file: no file
C                     5 = could not open extension file: file too busy
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   IVOL, ISLOT, IVER, ILUN, IFIND, IERR
      LOGICAL   EXCL, WAIT
C
      INTEGER   I
      LOGICAL   NOMAP
      CHARACTER FILE*48
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA NOMAP /.FALSE./
C-----------------------------------------------------------------------
C                                       Find latest version no.
      IF (IVER.LE.0) CALL FNDEXT (TYPE, CATBLK, IVER)
C                                       Error Ext. file not in header.
      IF (IVER.LE.0) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (7)
         END IF
C                                       Build file name.
 20   CALL ZPHFIL (TYPE, IVOL, ISLOT, IVER, FILE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) TYPE, IVOL, ISLOT, IVER
         CALL MSGWRT (7)
         IERR = 1
C                                       Open extension file.
      ELSE
         CALL ZOPEN (ILUN, IFIND, IVOL, FILE, NOMAP, EXCL, WAIT, I)
         IF (I.NE.0) THEN
            IERR = 3
            IF (I.EQ.2) IERR = 4
            IF (I.EQ.4) IERR = 5
            WRITE (MSGTXT,1030) I, TYPE, IVOL, ISLOT, IVER
            CALL MSGWRT (7)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR. EXTENSION FILE NOT IN MAP HEADER.')
 1020 FORMAT ('ERROR. COULD NOT BUILD EXT. FILE NAME',A2,I4,I7,I7)
 1030 FORMAT ('ERROR',I4,' OPENING EXTENSION FILE ',A2,I1,I5,I4)
      END
