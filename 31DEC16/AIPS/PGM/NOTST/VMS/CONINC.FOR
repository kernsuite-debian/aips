      PROGRAM CONINC
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
C   Program to concatenate C*.INC and E*.INC with D*.INC
C      Reads CONINC.LIS for a list of files to process, one name
C   per line e.g. "DMSG.INC;27"
C      The input files are assumed to have names of the form "Dnnn.INC"
C   with corresponding Cnnn.INC and Ennn.INC files (either or both may
C   be absent).
C      Output will be named Dnnn.NEW.
C      All files are assumed to be in the current default directory.
C   NOTE: The open statements in this routine are appropriate for VMS,
C   More generic versions are commented out and may be substituted for
C   non VMS systems.
C-----------------------------------------------------------------------
      CHARACTER*80 INLINE, OUTLIN, LASTLN
      CHARACTER*59 CSPACE
      CHARACTER*20 INFIL, IN2FIL, OUTFIL
      CHARACTER*20 INCNAM, INC
      INTEGER      POINT, IOERR, INDEX, ITRIM, LSTR
      LOGICAL      EXISTS
C-----------------------------------------------------------------------
      CSPACE =
     *   'C                                                          '
      INQUIRE (FILE='CONINC.LIS',EXIST=EXISTS)
      IF (.NOT.EXISTS) THEN
         WRITE (6,1001)
         GO TO 999
         END IF
C                                       VMS
      OPEN (UNIT=5,FILE='CONINC.LIS',READONLY,STATUS='OLD')
C                                       Other
C      OPEN (UNIT=5,FILE='CONINC.LIS',STATUS='OLD')
 100  READ(5,1000,END=999) INCNAM
      IF (INCNAM.EQ.' ') GO TO 999
      POINT = INDEX (INCNAM,'.')
      INC = INCNAM(1:POINT)
      INFIL = INCNAM
      OUTFIL = 'D' // INCNAM(2:POINT-1) // '.NEW'
C                                       Open INPUT files
C                                       VMS
      OPEN (UNIT=8,FILE=INFIL,READONLY,IOSTAT=IOERR,STATUS='OLD')
C                                       Other
C      OPEN (UNIT=8,FILE=INFIL,IOSTAT=IOERR,STATUS='OLD')
      IF (IOERR.NE.0) GO TO 890
C                                       Open Output file
C                                       VMS
      OPEN (UNIT=11,FILE=OUTFIL,STATUS='NEW',IOSTAT=IOERR,
     *   FORM='FORMATTED',RECORDSIZE=80,RECORDTYPE='VARIABLE',
     *   BLOCKSIZE=80, CARRIAGECONTROL='LIST',
     *   ACCESS='SEQUENTIAL',ORGANIZATION='SEQUENTIAL')
C                                       Other
C      OPEN (UNIT=11,FILE=OUTFIL,STATUS='NEW',IOSTAT=IOERR,
C     *   FORM='FORMATTED',RECL=80, ACCESS='SEQUENTIAL')
      IF (IOERR.NE.0) GO TO 890
C                                       Copy D*.INC except last line
      READ (8,1120,END=900) OUTLIN
C                                       Substitute standard header
C                                       comment.
      IF (OUTLIN(1:1).EQ.'C') THEN
         OUTLIN = CSPACE // 'Include ' // INC
C                                       No comment at beginning
      ELSE
         INLINE = CSPACE // 'Include ' // INC
         LSTR = ITRIM (OUTLIN)
         WRITE (11,1120) INLINE(1:LSTR)
         END IF
C                                       Loop over file
 200     READ (8,1120,END=250) INLINE
         LSTR = ITRIM (OUTLIN)
         WRITE (11,1120) OUTLIN(1:LSTR)
         OUTLIN = INLINE
         GO TO 200
C                                       Save last line
 250  LASTLN = OUTLIN
C                                       Copy C*.INC except first and
C                                       last line (if comments)
      IN2FIL = 'C' // INCNAM(2:POINT-1) // '.INC'
C                                       VMS
      OPEN (UNIT=9,FILE=IN2FIL,READONLY,IOSTAT=IOERR,STATUS='OLD')
C                                       Other
C      OPEN (UNIT=9,FILE=IN2FIL,IOSTAT=IOERR,STATUS='OLD')
      IF (IOERR.NE.0) GO TO 380
      READ (9,1120,END=900) OUTLIN
      LSTR = ITRIM (OUTLIN)
      IF (OUTLIN(1:1).NE.'C') WRITE (11,1120) OUTLIN(1:LSTR)
      READ (9,1120,END=900) OUTLIN
 300     READ (9,1120,END=350) INLINE
         LSTR = ITRIM (OUTLIN)
         WRITE (11,1120) OUTLIN(1:LSTR)
         OUTLIN = INLINE
         GO TO 300
 350  CLOSE (UNIT=9)
      LSTR = ITRIM (OUTLIN)
      IF (OUTLIN(1:1).NE.'C') WRITE (11,1120) OUTLIN(1:LSTR)
C                                       Copy E*.INC except first and
C                                       last line (if comments)
 380  IN2FIL = 'E' // INCNAM(2:POINT-1) // '.INC'
C                                       VMS
      OPEN (UNIT=9,FILE=IN2FIL,READONLY,IOSTAT=IOERR,STATUS='OLD')
C                                       Other
C      OPEN (UNIT=9,FILE=IN2FIL,IOSTAT=IOERR,STATUS='OLD')
      IF (IOERR.NE.0) GO TO 500
      READ (9,1120,END=900) OUTLIN
      LSTR = ITRIM (OUTLIN)
      IF (OUTLIN(1:1).NE.'C') WRITE (11,1120) OUTLIN(1:LSTR)
      READ (9,1120,END=900) OUTLIN
 400     READ (9,1120,END=450) INLINE
         LSTR = ITRIM (OUTLIN)
         WRITE (11,1120) OUTLIN(1:LSTR)
         OUTLIN = INLINE
         GO TO 400
 450  CLOSE (UNIT=9)
      LSTR = ITRIM (OUTLIN)
      IF (OUTLIN(1:1).NE.'C') WRITE (11,1120) OUTLIN(1:LSTR)
C                                       Write last line
C                                       Substitute standard end
C                                       comment.
 500  IF (LASTLN(1:1).EQ.'C') THEN
         LASTLN = CSPACE // 'End ' // INC
C                                       No comment at end
      ELSE
         LSTR = ITRIM (LASTLN)
         WRITE (11,1120) LASTLN(1:LSTR)
         LASTLN = CSPACE // 'End ' // INC
         END IF
      LSTR = ITRIM (LASTLN)
      WRITE (11,1120) LASTLN(1:LSTR)
      GO TO 900
C                                       Error opening file
 890  WRITE (6,1890) INCNAM
      GO TO 950
C                                       Finished this file
 900  WRITE (6,1900) INCNAM
      CLOSE (UNIT=11)
 950  CLOSE (UNIT=8)
C                                       Do next file.
 980  GO TO 100
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (A20)
 1001 FORMAT (' Error: file CONINC.LIS does not exist')
 1120 FORMAT (A)
 1890 FORMAT (' Error opening file ',A8)
 1900 FORMAT (' Finished with ',A8)
      END
      INTEGER FUNCTION ITRIM (STRING)
C-----------------------------------------------------------------------
C   Function to determine length of a string. I.e., it trims trailing
C   blanks. Use with calls like:
C         TRIMMED = GROSS(1:ITRIM(GROSS))
C   DCW, NRAO-CV, 30Nov82.
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
C-----------------------------------------------------------------------
      ITRIM = LEN (STRING) + 1
C                                       look backwards for non-blank
 10   CONTINUE
         ITRIM = ITRIM - 1
         IF (ITRIM.LT.1) GO TO 999
         IF (STRING(ITRIM:ITRIM).EQ.' ') GO TO 10
C
 999  RETURN
      END
