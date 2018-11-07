      SUBROUTINE LPOPEN (PNAME, DOCRT, LUN, FIND, NACROS, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! open appropriate device for printing
C# IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2002, 2004, 2009-2010
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
C   Open appropriate device for printing.
C   Inputs:
C      PNAME    C*48     the output text file to be kept
C                        (' ' => use a scratch for interactive AIPS's
C                        or use PRTFIL:BATCHjjj.nnn for batch jobs
C                        where jjj is the job number and nnn is the
C                        user number)
C   In/out:
C      DOCRT    R        > 0 => use terminal.  Forced < 0 on batch.
C   Output:
C      LUN      I        Logical unit number: 1 line printer, 3 disk
C                        text file, 5 terminal.
C      FIND     I        Index in FTAB to file control block for LUN
C      NACROS   I        Width to use in characters
C      SCRTCH   I(256)   Scratch buffer
C      IERR     I        Error return code: 0 => no error
C                           1 => error in inputs
C                           2 => LUN already in use
C                           3 => no room for LUN in FTAB
C                           4 => trouble translating logical
C                           5 => file already exists
C                           6 => open error
C   Uses ZLPOPN, ZTTOPN, ZTXOPN, ZFULLN, INQUIRE (EXIST), ZWINC
C-----------------------------------------------------------------------
      CHARACTER PNAME*(*)
      REAL      DOCRT
      INTEGER   LUN, FIND, NACROS, SCRTCH(256), IERR
C
      INTEGER   I, JOBN, NCOL
      CHARACTER LNAME*48
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------

      LNAME = PNAME
C                                       batch
      IF (NPOPS.GT.NINTRN+1) THEN
         DOCRT = MIN (-1.0, DOCRT)
         IF (LNAME.EQ.' ') THEN
            I = NPOPS - NINTRN - 1
            CALL BATQ ('WHOO', 0.0, ' ', I, 0, NLUSER, JOBN, SCRTCH,
     *         IERR)
            IF (IERR.NE.0) JOBN = 0
            LNAME = 'PRTFIL:BATCHjjj.nnn'
            CALL ZEHEX (JOBN, 3, LNAME(13:15))
            CALL ZEHEX (NLUSER, 3, LNAME(17:19))
            END IF
         END IF
C                                       terminal
      IF (DOCRT.GT.0.0) THEN
         LUN = 5
         CALL ZTTOPN (LUN, FIND, IERR)
         IF (IERR.NE.0) THEN
            IF (FTAB(FIND+1+NFCBER).GT.0) CALL ZERROR ('ZTTOPN',
     *         FTAB(FIND+1+NFCBER), ' ', -999, .FALSE.)
            WRITE (MSGTXT,1000) 'ZTTOPN', IERR
            IF (IERR.EQ.1) IERR = 6
            IERR = 1
            END IF
         CALL ZWINC (NCOL)
         NACROS = DOCRT + 0.5
         IF ((NACROS.LT.72) .OR. (NACROS.GT.NCOL)) NACROS = NCOL
C                                       Printer
      ELSE IF (LNAME.EQ.' ') THEN
         NACROS = NCHPRT
         IF (NACROS.LT.72) NACROS = 72
         LUN = 1
         CALL ZLPOPN (LUN, FIND, IERR)
         IF (IERR.NE.0) WRITE (MSGTXT,1000) 'ZLPOPN', IERR
C                                       Text file
      ELSE
         NACROS = 132
         LUN = 3
         CALL ZTXOPN ('WRIT', LUN, FIND, LNAME, .TRUE., IERR)
         IF (IERR.NE.0) WRITE (MSGTXT,1000) 'ZTXOPN', IERR
         END IF
C                                       Error
      IF (IERR.NE.0) CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LPOPEN: ',A,' RETURNS ERROR',I5)
      END
