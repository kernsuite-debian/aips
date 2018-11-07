      SUBROUTINE SGLOCA (IOP, IVER, FIXIT)
C-----------------------------------------------------------------------
C! locates a Save/Get file by name in catalog of SG files
C# POPS-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2004
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
C   SGLOCA takes care of locating a SAVE / GET file from the user's
C   input.  This routine is probably only of use to STORES.
C   Inputs:
C      IOP    I   1 => GET : will use min match, must exist
C                 2 => SAVE : exact match , new vers ok
C                 3 => GET LASTEXIT
C                 4 => SAVE LASTEXIT
C   Output:
C      IVER   I   Version # of S/G file (1 - 46655)
C      FIXIT  L   out of date version needs fixing?
C   Common:
C      POPS error common ERRNUM carries any error codes
C-----------------------------------------------------------------------
      INTEGER   IOP, IVER
      LOGICAL   FIXIT
C                                       Number of Names for matching
      INTEGER   MAXMAT
      PARAMETER (MAXMAT=150)
      CHARACTER PHNAME*48, PRGNAM*6, UNAME*16, TNAME*16
      INTEGER   IBLK(256), NWPL, NLPR, NCHAR, LCHAR, ITRIM, POTERR,
     *   IERR, IER, LUN, FIND, NTASK, LPNT, IREC, LREC, IMSP, SGVOK,
     *   IMSTK(MAXMAT), I, ISVERS, ISCHAR, ISIZE, IRRN, LSIZE, MPG(10),
     *   LPG(10)
      LOGICAL   T, F, N1WRIT, NEW
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:PKSZ.INC'
      DATA LUN /27/
      DATA PRGNAM /'SGLOCA'/
      DATA T, F /.TRUE.,.FALSE./
C                                       Old convertible ones:
C                                       14 can be COMPRESSed to 15
      DATA SGVOK /14/
C                                       MPAGE for vers 14...
      DATA MPG /150, 150, 388, 7*388/
C                                       LPAGE for vers 14...
      DATA LPG / 40,  40, 160, 7*160/
C-----------------------------------------------------------------------
      FIXIT = .FALSE.
C                                       set number of integers per
C                                       SAVE/GET entry
      NWPL = 7
C                                       number of SAVE/GET entries/block
      NLPR = 256 / NWPL
      IVER = 0
C                                       LASTEXIT implementation
      IF (IOP.GE.3) THEN
         UNAME = 'LASTEXIT'
         NCHAR = 8
C                                       Get user name
      ELSE
         CALL GETNME (' ', 16, UNAME)
         IF (ERRNUM.NE.0) GO TO 980
C                                       Count characters
         NCHAR = ITRIM (UNAME)
         POTERR = 8
         IF (NCHAR.LE.0) GO TO 980
         END IF
C                                       Directory file
      LCHAR = 16
      IF ((IOP.EQ.1) .OR. (IOP.EQ.3)) LCHAR = NCHAR
      CALL ZPHFIL ('SG', 1, NLUSER, 0, PHNAME, IERR)
      IER = 1
      POTERR = 101
C                                       "Create it" on Save
      CALL ZEXIST (1, PHNAME, LSIZE, IERR)
      NEW = IERR.EQ.1
      IF (NEW) THEN
         IF ((IOP.EQ.2) .OR. (IOP.EQ.4)) THEN
            ISIZE = 1000 / NLPR + 1
            CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IER)
            IF (IER.GE.2) THEN
               WRITE (MSGTXT,1020) IER
               CALL MSGWRT (6)
               GO TO 980
               END IF
         ELSE
            MSGTXT = 'NO SAVE/GET DIECTORY FOUND'
            IF (IOP.EQ.1) THEN
               CALL MSGWRT (6)
            ELSE
               POTERR = 45
               END IF
            GO TO 980
            END IF
      ELSE IF (IERR.GT.1) THEN
         WRITE (MSGTXT,1021) IERR
         CALL MSGWRT (6)
         GO TO 980
         END IF
      LSIZE = LSIZE * NLPR - 1
      LSIZE = MIN (LSIZE, 46655)
      CALL FILL (256, 0, IBLK)
C                                       Open directory
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 980
      POTERR = 50
C                                       Read old file
      IF (.NOT.NEW) THEN
         CALL ZFIO ('READ', LUN, FIND, 1, IBLK, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       The time, inits
      LREC = 3
      IF ((IOP.EQ.2) .OR. (IOP.EQ.4)) LREC = 5
      CALL CATIME (1, IBLK(LREC), IMSTK)
      N1WRIT = F
      LREC = 1
C                                     Number of names in SAVE/GET
      NTASK = IBLK(1)
      LPNT = NTASK + 1
      IMSP = 0
C                                       Look thru dir for name
      IF (NTASK.GT.0) THEN
         DO 50 I = 1,NTASK
            IRRN = I / NLPR + 1
            IF (IRRN.NE.LREC) THEN
C                                      Write only once
               IF (.NOT.N1WRIT) THEN
                  CALL ZFIO ('WRIT', LUN, FIND, 1, IBLK, IERR)
                  IF (IERR.NE.0) GO TO 970
                  N1WRIT = T
                  END IF
               CALL ZFIO ('READ', LUN, FIND, IRRN, IBLK, IERR)
               IF (IERR.NE.0) GO TO 970
               LREC = IRRN
               END IF
            IREC = MOD (I, NLPR) * NWPL + 1
C                                      read length of string
            ISCHAR = MOD (IBLK(IREC), 32)
C                                      if SAVE and input length <= name read
            IF (((IOP.EQ.2) .OR. (IOP.EQ.4)) .OR.
     *         (LCHAR.LE.ISCHAR)) THEN
C                                      name length is 0
               IF (IBLK(IREC).LE.0) THEN
C                                       reset number of names in SAVE/GET
                  LPNT = MIN (LPNT, I)
               ELSE
C                                       get SAVE/GET name
                  CALL H2CHR (ISCHAR, 1, IBLK(IREC+3), TNAME)
C                                      if name match up to L characters
                  IF (UNAME(1:LCHAR).EQ.TNAME(1:LCHAR)) THEN
C                                      if room in match array
                     IF (IMSP.LT.MAXMAT) THEN
C                                      keep track of matchs
                        IMSP = IMSP + 1
                        IMSTK(IMSP) = I
                        ISVERS = IBLK(IREC) / 32
                        END IF
C                                      if an exact match
                     IF (LCHAR.GE.ISCHAR) THEN
C                                      forget close names, record match
                        IMSP = 1
                        IMSTK(1) = I
C                                      end search
                        GO TO 60
                        END IF
                     END IF
                  END IF
               END IF
 50         CONTINUE
C                                       end if any files in dir.
         END IF
C                                       if GET operation
 60   IF ((IOP.EQ.1) .OR. (IOP.EQ.3)) THEN
         IF (.NOT.N1WRIT) CALL ZFIO ('WRIT', LUN, FIND, 1, IBLK, IERR)
         IF (IERR.NE.0) GO TO 970
         POTERR = 45
         IVER = IMSTK(1)
         IF (IMSP.EQ.1) THEN
            IF (ISVERS.LT.SGVERS) THEN
               POTERR = 30
               FIXIT = ISVERS.GE.SGVOK
               IF (FIXIT) THEN
                  MPAGE = MPG(ISVERS-SGVOK+1)
                  LPAGE = LPG(ISVERS-SGVOK+1)
                  END IF
            ELSE IF (ISVERS.GT.SGVERS) THEN
               POTERR = 3
            ELSE
               POTERR = 0
               END IF
            END IF
C                                       Not unique
         IF (IMSP.LE.1) GO TO 970
C                                       inform user
            MSGTXT = PRGNAM // ': SAVE/GET NAME ''' //
     *         UNAME(1:LCHAR) // ''' IS NOT UNIQUE '
            CALL MSGWRT (6)
C                                       for all matches
            DO 70 I = 1,IMSP
C                                       Which block has current name
               IRRN = IMSTK(I) / NLPR + 1
C                                       if current block not last block
               IF (IRRN.NE.LREC) THEN
                  CALL ZFIO ('READ', LUN, FIND, IRRN, IBLK, IERR)
                  IF (IERR.NE.0) GO TO 970
                  LREC = IRRN
                  END IF
C                                       location in block of name
               IREC = MOD (IMSTK(I), NLPR) * NWPL + 1
C                                      read length of string
               ISCHAR = MOD (IBLK(IREC), 32)
               CALL H2CHR (ISCHAR, 1, IBLK(IREC+3), TNAME)
               MSGTXT = PRGNAM // ': COULD BE ''' //
     *            TNAME(1:ISCHAR) // ''''
               CALL MSGWRT (6)
 70            CONTINUE
            GO TO 970
C                                       end if GET option
         END IF
C                                       else SAVE option
 75   IVER = LPNT
      IF (IMSP.EQ.1) IVER = IMSTK(1)
C                                       Too many versions
      IF (IVER.GT.LSIZE) THEN
         POTERR = 101
         WRITE (MSGTXT,1075) LSIZE
         CALL MSGWRT (6)
         GO TO 970
         END IF
C                                       Create/update entry
      IRRN = IVER / NLPR + 1
      I = MOD (IVER, NLPR) * NWPL + 1
C                                       if not already written
      IF ((.NOT.N1WRIT) .AND. (IRRN.NE.1)) THEN
         CALL ZFIO ('WRIT', LUN, FIND, 1, IBLK, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      IF (IRRN.NE.LREC) CALL ZFIO ('READ', LUN, FIND, IRRN, IBLK, IERR)
      IF (IERR.NE.0) GO TO 970
      IBLK(I) = NCHAR + 32 * SGVERS
      CALL CATIME (1, IBLK(I+1), IMSTK)
      CALL CHR2H (16, UNAME, 1, IBLK(I+3))
C                                       Save & get rec 1 back
      IF (IRRN.NE.1) THEN
         CALL ZFIO ('WRIT', LUN, FIND, IRRN, IBLK, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL ZFIO ('READ', LUN, FIND, 1, IBLK, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Update info
      IF (IVER.GT.NTASK) IBLK(1) = IVER
      IF ((IVER.LE.NTASK) .AND. (IMSP.EQ.0)) IBLK(2) = IBLK(2) - 1
      CALL ZFIO ('WRIT', LUN, FIND, 1, IBLK, IERR)
      IF (IERR.EQ.0) POTERR = 0
C                                       Close file
 970  CALL ZCLOSE (LUN, FIND, IERR)
C                                       error test
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('UNABLE TO CREATE DIRECTORY. IER =',I7)
 1021 FORMAT ('UNABLE TO ACCESS DIRECTORY. IER =',I7)
 1075 FORMAT ('ONLY',I6,' SAVE FILES ARE ALLOWED PER USER')
      END
