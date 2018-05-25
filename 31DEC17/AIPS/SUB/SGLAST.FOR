      SUBROUTINE SGLAST (IOP)
C-----------------------------------------------------------------------
C! does a SAVE or GET of the K array cataloged as LASTEXIT.
C# POPS-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2003-2004
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
C   SGLAST is an AIPS subroutine to GET or SAVE a special area called
C   LASTEXIT as a SAVE / GET file.
C   Input:
C      IOP   I     1 => get, 2 => save
C   Common scratch:
C      /AIPSCR/ I(4352)
C-----------------------------------------------------------------------
      INTEGER   IOP
C
      CHARACTER PHNAME*48, IOPRAT*4, PRGNAM*6, CNAME*12
      INTEGER   LOP, IERR, POTERR, I, J, IVER, JJ, LUNSG, FIND, LUN,
     *   ISIZE, LSIZE, IDPI
      LOGICAL   T, F, WASFIL, FIXIT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA LUNSG /27/
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'SGLAST'/
C-----------------------------------------------------------------------
      IOPRAT = 'READ'
      IF (IOP.EQ.2) IOPRAT = 'WRIT'
      WASFIL = IOP.NE.2
      LOP = IOP + 2
C                                       locate desired file
 15   CALL SGLOCA (LOP, IVER, FIXIT)
C                                       cannot do fixit
      IF ((FIXIT) .AND. (IOPRAT.EQ.'READ') .AND. (ERRNUM.EQ.30)) THEN
         ERRNUM = 0
         JBUFF = 'GET LASTEXIT'
         HOLDUF = JBUFF
         IUNIT = 4
C                                       reset file size
         LPAGE = LBLOCK
         MPAGE = KBLOCK + LPAGE
         MSGTXT = 'Warning: out of date LASTEXIT to be corrected' //
     *      ' by GET LASTEXIT'
         CALL MSGWRT (6)
         GO TO 999
         END IF
      IF (ERRNUM.NE.0) GO TO 970
      POTERR = 0
      IF ((LOP.EQ.3) .AND. (IVER.EQ.0)) GO TO 970
      CALL ZPHFIL ('SG', 1, NLUSER, IVER, PHNAME, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       create file if needed
      IF (IOPRAT.EQ.'WRIT') THEN
         ISIZE = MPAGE + 1
         CALL ZEXIST (1, PHNAME, LSIZE, IERR)
         IF (IERR.EQ.0) CALL ZDESTR (1, PHNAME, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.1)) THEN
            POTERR = 50
            GO TO 970
            END IF
C                                       now create it
 20      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.1)) THEN
            POTERR = 50
            GO TO 970
            END IF
         END IF
C                                       open SG file
 25   CALL ZOPEN (LUNSG, FIND, 1, PHNAME, F, T, T, IERR)
      POTERR = 101
      IF (IERR.NE.0) GO TO 965
      LUN = LUNSG
C                                       move K array
      JJ = MPAGE - LPAGE
      POTERR = 61
      IDPI = 2
      J = 1
      DO 35 I = 1,JJ
         CALL ZFIO (IOPRAT, LUN, FIND, IDPI, K(J), IERROR)
         IF (IERROR(1).NE.0) GO TO 960
         IDPI = IDPI + 1
         J = J + 256
 35      CONTINUE
C                                       move LISTF
      J = 1
      DO 40 I = 1,LPAGE
         CALL ZFIO (IOPRAT, LUN, FIND, IDPI, LISTF(J), IERROR)
         IF (IERROR(1).NE.0) GO TO 960
         IDPI = IDPI + 1
         J = J + 256
 40      CONTINUE
C                                       SG files: write time
      IF (IOP.EQ.2) THEN
         CALL FILL (256, 0, LISTF)
         CALL ZDATE (LISTF(1))
         CALL ZTIME (LISTF(4))
         IDPI = 1
         CALL ZFIO ('WRIT', LUNSG, FIND, IDPI, LISTF, IERR)
         END IF
C                                       Close down: normal
 950  POTERR = 0
      WASFIL = .TRUE.
      IF (IOP.EQ.2) THEN
         WRITE (MSGTXT,1951)
         CALL MSGWRT (3)
C                                       read - check quality
      ELSE
         CALL ADVERB ('INSEQ', 'I', 1, 1, I, I, CNAME)
         IF (ERRNUM.EQ.0) CALL ADVERB ('INNAME', 'C', 1, 12, I, I,
     *      CNAME)
         IF (ERRNUM.EQ.0) THEN
            WRITE (MSGTXT,1950)
            CALL MSGWRT (3)
         ELSE
            MSGTXT = 'READ LASTEXIT FILE OKAY, BUT CONTENTS WRONG'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       close SG
 960  CALL ZCLOSE (LUNSG, FIND, IERR)
C                                       destroy new SG on err
 965  IF (.NOT.WASFIL) CALL ZDESTR (1, PHNAME, IERR)
C                                       close file ME
 970  CONTINUE
C
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         WRITE (MSGTXT,1980) IOPRAT
C
 999  RETURN
C-----------------------------------------------------------------------
 1950 FORMAT ('Recovered POPS environment from last exit')
 1951 FORMAT ('Saved POPS environment in area named ''LASTEXIT''')
 1980 FORMAT ('UNABLE TO ',A,' SAVE/GET FILE NAMED LASTEXIT.  CHECK',
     *   ' $DA01:SG*')
      END
