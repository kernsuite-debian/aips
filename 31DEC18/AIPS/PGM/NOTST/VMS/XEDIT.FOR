LOCAL INCLUDE 'XEDIT.INC'
C                                                            Begin XEDIT
C                                       Local include for XEDIT
      CHARACTER OLDSTR(500)*50, NEWSTR(500)*50, OLDCAL(100)*100,
     *   NEWCAL(100)*250, HOLDIM(100)*20, LOGSTR(100)*20,
     *   LOGREA(100)*25, HOLDCL(200)*6, CHRDCL(500)*6,
     *   HKEEP(100)*6, HKDIM(100)*20, CKEEP(100)*6, CKDIM(100)*20,
     *   COPYRI*200, CMINUS*72
      INTEGER   LOLD(500), LNEW(500), LOLDC(100), LNEWC(100), LLOG(100),
     *   LSRC(100), NCSUB(100), LHOL(200), LCHR(500), HOLARG(5,200),
     *   CHRARG(5,500,2),
     *   NUMSUB, NUMCAL, NUMLOG, NUMHOL, NUMCHR, NHKEEP, NCKEEP,
     *   ICHA, ICHZ, BEGDCL, ENDDCL, BEGFOR, NUMDCL, DCLPOS(3,100),
     *   SCOUNT, CCOUNT
      LOGICAL   DOHOLL(100)
      COMMON /CHRCOM/ OLDSTR, NEWSTR, OLDCAL, NEWCAL, HOLDIM, LOGSTR,
     *   LOGREA, HOLDCL, CHRDCL, HKEEP, HKDIM, CKEEP, CKDIM, COPYRI,
     *   CMINUS
      COMMON /INTCOM/ LOLD, LNEW, LOLDC, LNEWC, LLOG, LSRC, NCSUB,
     *   LHOL, LCHR, HOLARG, CHRARG,
     *   NUMSUB, NUMCAL, NUMLOG, NUMHOL, NUMCHR, NHKEEP, NCKEEP,
     *   ICHA, ICHZ,
     *   BEGDCL, ENDDCL, BEGFOR, NUMDCL, DCLPOS, SCOUNT, CCOUNT,
     *   DOHOLL
C                                                            End XEDIT
LOCAL END
      PROGRAM XEDIT
C-----------------------------------------------------------------------
C! Transforms Fortran source files using a list of instructions
C# Service Text
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
C   Program to make text substitutions to a list of files etc.
C   Reads XEDIT.LIS for a list of files to process, one name
C   per line.; the input files are assumed to have the name "name.FOR"
C   and the new routines  will have the name "name.NEW".
C   Instructions to XEDIT are read from XEDIT.STR  and are of the form:
C   'opcode' 'string1' 'string2'         all on the same line
C    OPCODE = 'copyright' => copyright notice if any.
C          string1 = text of copyright notice; a '@' means a new line
C             with the same indentation as the first. 100 char max.
C          string2 = ''  continuation of string1 but MUST be present,
C             may add an additional 100 characters.
C    OPCODE ='subs' => simple string substitution;  any occurances of
C       the string1 will be replaced by string2.  Each string may be up
C       to 50 characters.
C    OPCODE ='call' => edit subroutine call sequence/array argument:
C       string1 is of the form 'CALL XYZ (,,,arg,)' where arg
C          is an argument name required for a match.  Leading commas
C          indicate the argument number.  If the argument list is not
C          given then no test is made on the number of arguments.  If
C          the list of arguments is given then the number of arguments
C          must match.  Arguments may be of the form !xxxx* where the
C          '!' indicates that the actual argument be different to match
C          and '*' indicates any number of wildcard characters (* may
C          only appear at the end. Include a blank before the initial
C          '(' for call sequences and leave it out for array elements.
C          If string1='continue' then string2 is a continuation of the
C          previous 'call' or 'callh' command.
C          max. = 100 characters
C       string2 is of the form 'buzzbuzz ($01, $02, $03, $05) %'
C          i.e. an occurance of the string $nn will result in argument
C          nn being substituted.  A '@' will force a new line with the
C          same indentation as the previous line.  A substituted line
C          has a maximum of 512 characters.
C          Maximum of 250 characters.
C          The optional % indicates that the rest of the line past the
C          final parenthese parsed is added.
C    OPCODE ='callh (dim)' => like 'call' except for array arguments
C       only and the array is to be declared HOLLERITH.  (dim) is the
C       default dimension.
C    OPCODE ='log' => writes any line in the log file with a substring
C       matching string1.  A "reason" code of up to 10 char is given
C       in string2 must be provided.  String1 has a mamimum of
C       20 characters.
C    OPCODE ='comment' is ignored; string1 and string2 must be present
C       but may contain comments.
C    Each line will be operated on in the sequence described above.
C    When the above processing is done the declaration processor may
C    be invoked:
C    OPCODE ='hollerith' => changes the declarations of local variables
C          which are the specified arguments of specified routines to
C          HOLLERITH.
C          string1 = 'routine name'      6 characters only
C          string2 = '  j  i  0  0  0'  is a list of the argument
C                   numbers which are Hollerith variables.  These are
C                   read with an 5I3 format.
C    OPCODE ='keep hollerith' => specifies a variable name to be
C          declared HOLLERITH if used in a call sequence even it it is
C          not already locally defined.  Limit 50.
C          string1 = 'variable name'     6 characters only
C          string2 = 'dimension'        blank or of form '(5)' 20 char.
C    OPCODE ='character' => changes the declarations of local variables
C          which are the specified arguments of specified routines to
C          CHARACTER.
C          string1 = 'routine name'       6 characters only
C          string2 = ' j1 j2  i1 i2  0  0  0  0  0  0  0'
C                   is a list of the argument numbers which are
C                   CHARACTER variables followed by the number of
C                   characters to be declared.  (j1=arg number,
C                   j2 = max. string size)
C                   These are read with an 10I3 format.
C    OPCODE ='keep character' => specifies a variable name to be
C          declared CHARACTER if used in a call sequence even it it is
C          not already locally defined.  Limit 50.
C          string1 = 'variable name'     6 characters only
C          string2 = 'def. decl. string e.g. "*4"'    20 char limit.
C      If the output line is longer than 72 characters then it will be
C   broken into shorter lines with an attempt at proper indentation.
C   NOTE: The open statements in this routine are appropriate for VMS,
C   More generic versions are commented out and may be substituted for
C   non VMS systems.
C-----------------------------------------------------------------------
      INTEGER   XMODSZ
C                                       XMODSZ=no. pages in module
C                                       buffer.
      PARAMETER (XMODSZ = 4)
      CHARACTER INLINE*100, OUTLIN*1024, INFIL*20, OUTFIL*20, LOGFIL*20,
     *   SUBNAM*20, TSTR*1024, MODNAM*40, MODULE(XMODSZ)*32767,
     *   ARGHOL(500)*6, PRECOM*200
      INTEGER      LOOP, BLNK1, NARGI, NARGO, NSAVE,
     *   I, LSTR, ITRIM, IOERR, LI, LO, IPNT, JPNT, KPNT, JSTR, LCNT,
     *   INDEX, LAST, LMODUL(4), PAGE, IERR, LIMPAG(XMODSZ), LTEMP,
     *   NHARG, NSAVE2, LCOUNT, LPREC, NEXTNQ
      LOGICAL   GOTMOD, MODONE, DUNPRE, PASTCM, FILDUN
      INCLUDE 'XEDIT.INC'
      EXTERNAL NEXTNQ
      DATA LIMPAG /XMODSZ*32000/
C-----------------------------------------------------------------------
C                                       Build description
      LIMPAG(1) = 25000
C                                       Get substitute strings
      CALL SETUP (IERR)
      IF (IERR.NE.0) GO TO 999
      NSAVE = NHKEEP
      NSAVE2 = NUMSUB
C                                       Get list of routines
C                                       VMS
      OPEN (UNIT=5,FILE='XEDIT.LIS',READONLY,STATUS='OLD')
C                                       Other
C      OPEN (UNIT=5,FILE='XEDIT.LIS',STATUS='OLD')
 100     READ(5,1000,END=999) SUBNAM
         IF (SUBNAM.EQ.' ') GO TO 999
C                                       Find first blank or '.'
         BLNK1 = INDEX (SUBNAM,'.')
         IF (BLNK1.LE.0) BLNK1 = INDEX (SUBNAM,' ')
C                                       Form input and output names
         INFIL = SUBNAM(1:BLNK1-1)//'.FOR'
         OUTFIL = SUBNAM(1:BLNK1-1)//'.NEW'
         LOGFIL = SUBNAM(1:BLNK1-1)//'.LOG'
C                                       Open input file
C                                       VMS
         OPEN (UNIT=10,FILE=INFIL,READONLY,IOSTAT=IOERR,STATUS='OLD')
C                                       Other
C         OPEN (UNIT=10,FILE=INFIL,IOSTAT=IOERR,STATUS='OLD')
         IF (IOERR.NE.0) GO TO 890
C                                       Open Output file
C                                       VMS
         OPEN (UNIT=11,FILE=OUTFIL,STATUS='NEW',IOSTAT=IOERR,
     *      FORM='FORMATTED',RECORDSIZE=80,RECORDTYPE='VARIABLE',
     *      BLOCKSIZE=80, CARRIAGECONTROL='LIST',
     *      ACCESS='SEQUENTIAL',ORGANIZATION='SEQUENTIAL')
C                                       Other
C         OPEN (UNIT=11,FILE=OUTFIL,STATUS='NEW',IOSTAT=IOERR,
C     *      FORM='FORMATTED',RECL=80, ACCESS='SEQUENTIAL')
         IF (IOERR.NE.0) GO TO 890
C                                       Open log file
C                                       VMS
         OPEN (UNIT=12,FILE=LOGFIL,STATUS='NEW',IOSTAT=IOERR,
     *      FORM='FORMATTED',RECORDSIZE=132,RECORDTYPE='VARIABLE',
     *      BLOCKSIZE=80, CARRIAGECONTROL='LIST',
     *      ACCESS='SEQUENTIAL',ORGANIZATION='SEQUENTIAL')
C                                       Other
C         OPEN (UNIT=12,FILE=LOGFIL,STATUS='NEW',IOSTAT=IOERR,
C     *      FORM='FORMATTED',RECL=132, ACCESS='SEQUENTIAL')
         IF (IOERR.NE.0) GO TO 890
C                                       Label log
         WRITE (12,1002) INFIL
C-----------------------------------------------------------------------
         SCOUNT = 0
         LCOUNT = 0
         CCOUNT = 0
         GOTMOD = .FALSE.
         MODNAM = 'no name'
         MODONE = .FALSE.
         FILDUN = .FALSE.
         DUNPRE = .FALSE.
         PASTCM = .FALSE.
         LMODUL(1) = 1
         LMODUL(2) = 1
         LMODUL(3) = 1
         LMODUL(4) = 1
         PAGE = 1
         NUMSUB = NSAVE2
C                                       Crunch file
C                                       Read line
 200        CALL GETLIN (OUTLIN, LSTR, IOERR)
            IF (IOERR.EQ.4) GO TO 600
C                                       Get module name
            IF (.NOT.GOTMOD) THEN
               IPNT = INDEX (OUTLIN, 'PROGRAM ')
               IF (IPNT.LE.0) IPNT = INDEX (OUTLIN, 'SUBROUTINE ')
               IF (IPNT.LE.0) IPNT = INDEX (OUTLIN, 'FUNCTION ')
               IF (IPNT.LE.0) IPNT = INDEX (OUTLIN, 'LOCAL INCLUDE')
               IF (IPNT.GT.1) THEN
                  KPNT = INDEX (OUTLIN(1:IPNT+25),'(') - 1
                  IF (KPNT.LE.0) KPNT = LSTR
                  MODNAM = OUTLIN(7:KPNT)
                  NHKEEP = NSAVE
C                                       Annotate log
                  JSTR = ITRIM (MODNAM)
                  WRITE (12,1001) MODNAM(1:JSTR)
                  GOTMOD = .TRUE.
                  END IF
               END IF
C                                       Check for END of module
            KPNT = INDEX (OUTLIN(1:LSTR), ' END')
            IF ((KPNT.GT.0) .AND. (KPNT.EQ.(LSTR-3)) .AND.
     *         (OUTLIN(1:1).NE.'C') .AND.
     *         (ITRIM (OUTLIN(6:KPNT)).EQ.0)) THEN
               GOTMOD = .FALSE.
               MODONE = .TRUE.
               MODNAM ='no name'
               END IF
C                                       Check for precursor comments etc
            IF (PASTCM .AND. (OUTLIN(1:2).NE.'C!') .AND.
     *         (.NOT.DUNPRE)) THEN
               DUNPRE = .TRUE.
C                                       Add to output
               PRECOM(1:5) = 'C!   '
               LPREC = 2
               CALL LINOUT (PRECOM, LPREC, MODULE(PAGE), LMODUL(PAGE))
               PRECOM(1:5) = 'C#   '
               CALL LINOUT (PRECOM, LPREC, MODULE(PAGE), LMODUL(PAGE))
               JSTR = ITRIM (COPYRI)
               IF (JSTR.GT.0) THEN
                  PRECOM(1:JSTR) = COPYRI(1:JSTR)
                  CALL LINOUT (PRECOM, JSTR, MODULE(PAGE), LMODUL(PAGE))
                  END IF
               LPREC = 72
               CALL LINOUT (CMINUS, LPREC, MODULE(PAGE), LMODUL(PAGE))
               END IF
            DUNPRE = DUNPRE .OR. (OUTLIN(1:2).EQ.'C!')
            PASTCM = PASTCM .OR. (OUTLIN(1:30).EQ.CMINUS(1:30))
C                                       Call sequence/array argument
C                                       processing.
            IF ((NUMCAL.GT.0) .AND. (LSTR.GT.10) .AND.
     *         (OUTLIN(1:1).NE.'C') ) THEN
               IF ((INDEX (OUTLIN(7:LSTR), '(').GT.0) .AND.
     *            (INDEX (OUTLIN(6:15), ' FORMAT') .LE.0))
     *               CALL CALLSQ (OUTLIN, LSTR, NHARG, ARGHOL)
               END IF
C                                       Line to MODULE
C                                       See if line break
 400       KPNT = NEXTNQ (OUTLIN(1:LSTR),'@')
C                                       Simple line
            IF ((LSTR.LE.72) .AND. (KPNT.LE.0)) THEN
               LCNT = LMODUL(PAGE)
               MODULE(PAGE)(LCNT:LCNT+LSTR-1) = OUTLIN(1:LSTR)
               MODULE(PAGE)(LCNT+LSTR:LCNT+LSTR) = '@'
               LMODUL(PAGE) = LMODUL(PAGE) + LSTR + 1
C                                       Complex case
            ELSE
               CALL LINOUT (OUTLIN, LSTR, MODULE(PAGE), LMODUL(PAGE))
               END IF
C                                       See if module page full
            IF (LMODUL(PAGE).GT.LIMPAG(PAGE)) THEN
C                                       New page
               PAGE = PAGE + 1
               IF (PAGE.GT.XMODSZ) THEN
                  WRITE(6,1400)
                  END IF
               END IF
            GO TO 610
C                                       End of Module - write
 600        FILDUN = .TRUE.
            MODONE = .TRUE.
            IF ((PAGE.EQ.1) .AND. (LMODUL(1).LE.1)) GO TO 900
C                                       See if module finished
 610        IF (MODONE) THEN
C                                       Log requested lines
               CALL LOGIT (PAGE, MODULE, LMODUL, NUMLOG, LOGSTR, LLOG,
     *            LOGREA)
C                                       Find beginning and end of
C                                       declarations, executable
C                                       Assumes declarations on first
C                                       page.
               CALL FNDDCL (MODULE, LMODUL)
C                                       Declaration processing
               IF ((NUMHOL.GT.0) .OR. (NUMCHR.GT.0)) THEN
                  CALL DCLEDT (PAGE, MODULE, LMODUL, NHARG, ARGHOL)
                  END IF
C                                       Remove dead declarations
               CALL RMDCL (PAGE, MODULE, LMODUL)
C                                       Write module to output file
               CALL PRTMOD (PAGE, MODULE, LMODUL, LCOUNT)
C                                       Clear work arrays
               DO 700 LOOP = 1,PAGE
                  MODULE(LOOP)(1:LMODUL(LOOP)) = '  '
                  LMODUL(LOOP) = 1
 700              CONTINUE
               PAGE = 1
               MODONE = .FALSE.
               NHARG = 0
               END IF
C                                       File done?
            IF (FILDUN) GO TO 900
C                                       Loop for next line
            GO TO 200
C                                       Error opening file
 890     WRITE (6,1890) SUBNAM(1:BLNK1-1)
         GO TO 950
C                                       Finished this file
 900     WRITE (6,1900) SUBNAM(1:BLNK1-1), SCOUNT
         WRITE (6,1901) CCOUNT
         WRITE (6,1902) LCOUNT
         WRITE (12,1900) SUBNAM(1:BLNK1-1), SCOUNT
         WRITE (12,1901) CCOUNT
         WRITE (12,1902) LCOUNT
         CLOSE (UNIT=11,STATUS='KEEP')
         CLOSE (UNIT=12,STATUS='KEEP')
 950     CLOSE (UNIT=10,STATUS='KEEP')
C                                       Do next file.
         GO TO 100
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (A20)
 1001 FORMAT (//' In module ',A)
 1002 FORMAT (30X,'XEDIT log file for ',A///)
 1120 FORMAT (A)
 1250 FORMAT (A)
 1400 FORMAT (' HELP, You''ve blown my module buffer;',
     *   ' increase the dimension of MODULE.')
 1890 FORMAT (' Error opening file ',A6)
 1900 FORMAT (' Finished with ',A6,', made ',I6,' substitutions')
 1901 FORMAT (' Made ',I6,' call translations')
 1902 FORMAT (' Wrote ',I6, ' output lines.')
      END
      INTEGER FUNCTION ITRIM (STRING)
C-----------------------------------------------------------------------
C   Function to determine length of a string. I.e., it trims trailing
C   blanks. Use with calls like:
C         TRIMMED = GROSS(1:ITRIM(GROSS))
C   DCW, NRAO-CV, 30Nov82.
C   WARNING: this function must be declared INTEGER not INTEGER*2
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
      INTEGER FUNCTION LAST (STRING, CHAR)
C-----------------------------------------------------------------------
C   Function to determine last occurance of a character in a string.
C    Inputs:
C     STRING    C   String to be searched
C     CHAR      C   Character to locate
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), CHAR*1
C-----------------------------------------------------------------------
      LAST = LEN (STRING) + 1
C                                       look backwards for CHAR
 10   CONTINUE
         LAST = LAST - 1
         IF (LAST.LT.1) GO TO 999
         IF (STRING(LAST:LAST).NE.CHAR) GO TO 10
C
 999  RETURN
      END
      SUBROUTINE SETUP (IERR)
C-----------------------------------------------------------------------
C   Function to read editing instructions
C    Output:
C     IERR      I  Error code 0 => OK else failed
C    Output in common:
C     COPYRI    C  Copyright notice if any
C     NUMSUB    I  Number of substitution string pairs.
C     OLDSTR(*) C  Array of substitution search strings
C     LOLD(*)   I  Length of elements in OLDSTR
C     NEWSTR(*) C  Array of substitution replacment strings
C     LNEW(*)   I  Length of elements in NEWSTR
C     NUMCAL    I  Number of call translation string pairs.
C     OLDCAL(*) C  Array of call translation search strings
C     LOLDC(*)  I  Length of elements in OLDCAL
C     NEWCAL(*) C  Array of call translation replacment strings
C     LNEWC(*)  I  Length of elements in NEWCAL
C     LSRC(*)   I  Number of characters in OLDCAL to compare
C     DOHOLL(*) L  If true declare associated OLDCAL array HOLLERITH
C     HOLDIM(*) C  Default declarations for 'calld' varaibles
C     NCSUB(*)  I  Number of parameter substitutions in call processing
C     NUMLOG    I  Number of logging string pairs.
C     LOGSTR(*) C  Array of logging search strings
C     LOGREA(*) C  Reason codes for LOGSTR
C     LLOG(*)   I  Length of elements in LOGSTR
C     NUMHOL    I  Number of Hollerith redeclaration routines
C     HOLDCL(*) C  Array of Hollerith routines
C     LHOL(*)   I  Length of elements in HOLDCL
C     HOLARG()  I  Arguments for Hollerith redeclarations
C     NHKEEP    I  Number of values in HKEEP
C     HKEEP     C  Array of HOLLERITH variables to declare if used.
C     NUMCHR    I  Number of Character redeclaration routines
C     CHRDCL(*) C  Array of Character routines
C     LCHR(*)   I  Length of elements in CHRDCL
C     CHRARG()  I  Arguments for Character redeclarations
C     NCKEEP    I  Number of values in CKEEP
C     CKEEP     C  Array of CHARACTER variables to declare if used.
C     ICHA      I  Value of the letter 'A'
C     ICHZ      I  Value of the letter 'Z'
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER OPCODE*20, STRNG1*100, STRNG2*250
      INTEGER   IPNT, KPNT, ITRIM, J, LCOUNT
      INCLUDE 'XEDIT.INC'
C-----------------------------------------------------------------------
      COPYRI = '          '
      CMINUS = 'C------------------------------------------------------'
      CMINUS(56:72) = '-----------------'
C                                       Letter values
      ICHA = ICHAR ('A')
      ICHZ = ICHAR ('Z')
      NUMSUB = 0
      NUMCAL = 0
      NUMLOG = 0
      NUMHOL = 0
      NUMCHR = 0
      NHKEEP = 0
      NCKEEP = 0
      LCOUNT = 1
C                                       VMS
      OPEN (UNIT=5,FILE='XEDIT.STR',READONLY,STATUS='OLD')
C                                       Other
C      OPEN (UNIT=5,FILE='XEDIT.STR',STATUS='OLD')
 10      READ(5,*,END=50,ERR=900)  OPCODE, STRNG1, STRNG2
         LCOUNT = LCOUNT + 1
C                                       Copyright notice
         IF (OPCODE(1:9).EQ.'copyright') THEN
            COPYRI(1:100) = STRNG1
            IPNT = ITRIM (COPYRI(1:100))
            COPYRI(IPNT+1:IPNT+100) = STRNG2
            END IF
C                                       Simple substitution
         IF (OPCODE(1:4).EQ.'subs') THEN
             NUMSUB = NUMSUB + 1
             OLDSTR(NUMSUB) = STRNG1(1:50)
             NEWSTR(NUMSUB) = STRNG2(1:50)
             LOLD(NUMSUB) = ITRIM (OLDSTR(NUMSUB))
             LNEW(NUMSUB) = ITRIM (NEWSTR(NUMSUB))
             IF (LOLD(NUMSUB).LE.0) GO TO 10
             END IF
C                                       Call translation
         IF (OPCODE(1:4).EQ.'call') THEN
             NUMCAL = NUMCAL + 1
             OLDCAL(NUMCAL) = STRNG1(1:100)
             NEWCAL(NUMCAL) = STRNG2(1:250)
             LOLDC(NUMCAL) = ITRIM (OLDCAL(NUMCAL))
             LNEWC(NUMCAL) = ITRIM (NEWCAL(NUMCAL))
             DOHOLL(NUMCAL) = OPCODE(5:5).EQ.'h'
             HOLDIM(NUMCAL) = OPCODE(6:20)
             IF (LOLDC(NUMCAL).LE.0) GO TO 10
             LSRC(NUMCAL) = INDEX (OLDCAL(NUMCAL)(1:LOLDC(NUMCAL)),
     *          ' (') - 1
C                                       Include '('
             IF (LSRC(NUMCAL).LE.0)
     *          LSRC(NUMCAL) = INDEX (OLDCAL(NUMCAL)(1:LOLDC(NUMCAL)),
     *          '(')
             IF (LSRC(NUMCAL).LE.0) LSRC(NUMCAL) = LOLDC(NUMCAL)
C                                       Number of parameters used
             IPNT = 1
             NCSUB(NUMCAL) = 0
 30             KPNT = INDEX (NEWCAL(NUMCAL)(IPNT:LNEWC(NUMCAL)), '$')
                IF (KPNT.GT.0) THEN
                   NCSUB(NUMCAL) = NCSUB(NUMCAL) + 1
                   IPNT = IPNT + KPNT
                   GO TO 30
                   END IF
             END IF
C                                       Logging strings
         IF (OPCODE(1:3).EQ.'log') THEN
             NUMLOG = NUMLOG + 1
             LOGSTR(NUMLOG) = STRNG1(1:20)
             LOGREA(NUMLOG) = STRNG2(1:25)
             LLOG(NUMLOG) = ITRIM (LOGSTR(NUMLOG))
             IF (LLOG(NUMLOG).LE.0) GO TO 50
             END IF
C                                       Hollerith redeclaration
         IF (OPCODE(1:9).EQ.'hollerith') THEN
            NUMHOL = NUMHOL + 1
            HOLDCL(NUMHOL) = STRNG1(1:6)
            READ (STRNG2,1030) (HOLARG(J,NUMHOL), J=1,5)
            END IF
C                                       Holleriths to declare if used
         IF (OPCODE(1:14).EQ.'keep hollerith') THEN
            NHKEEP = NHKEEP + 1
            HKEEP(NHKEEP) = STRNG1(1:6)
            HKDIM(NHKEEP) = STRNG2(1:20)
            END IF
C                                       Character redeclaration
         IF (OPCODE(1:9).EQ.'character') THEN
            NUMCHR = NUMCHR + 1
            CHRDCL(NUMCHR) = STRNG1(1:6)
            READ (STRNG2,1030) (CHRARG(J,NUMCHR,1),
     *         CHRARG(J,NUMCHR,2), J=1,5)
            END IF
C                                       Characters to declare if used
         IF (OPCODE(1:14).EQ.'keep character') THEN
            NCKEEP = NCKEEP + 1
            CKEEP(NCKEEP) = STRNG1(1:6)
            CKDIM(NCKEEP) = STRNG2(1:20)
            END IF
         GO TO 10
 50   CLOSE (UNIT=5)
      IERR = 0
      GO TO 999
C                                       Error
 900  WRITE (6,1900) LCOUNT
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (10I3)
 1900 FORMAT (' Error reading instructions from XEDIT.STR in command',
     *   I5)
      END
      SUBROUTINE GETLIN (OUTLIN, LSTR, IERR)
C-----------------------------------------------------------------------
C   Read a line from the input file, AIPSify and do string substitutions
C    Input from common:
C     NUMSUB    I  Number of substitution string pairs.
C     OLDSTR(*) C  Array of substitution search strings
C     LOLD(*)   I  Length of elements in OLDSTR
C     NEWSTR(*) C  Array of substitution replacment strings
C     LNEW(*)   I  Length of elements in NEWSTR
C    Input/Output in common:
C     SCOUNT    I  Number of substitutions
C    Output:
C     OUTLIN    C  Output line
C     LSTR      I  Number of valid characters in OUTLIN
C     IERR      I  Error code, 0=OK 1 = Error, 4=EOF
C-----------------------------------------------------------------------
      CHARACTER OUTLIN*(*)
      INTEGER   LSTR, IERR
      CHARACTER INLINE*256, TSTR*1024
      INTEGER   ITRIM, IPNT, JPNT, KPNT, I, LTEMP
      INCLUDE 'XEDIT.INC'
C-----------------------------------------------------------------------
C                                       Read line
      READ (10,1000,ERR=500,END=600) INLINE
      LSTR = ITRIM (INLINE)
C                                       AIPSisfy
      CALL AIPCLN (INLINE, LSTR)
C                                       Copy to OUTLIN
      OUTLIN = INLINE(1:LSTR)
C                                       Make substitutions
      JPNT = 1
      DO 100 I = 1,NUMSUB
 50      KPNT = INDEX (OUTLIN(JPNT:LSTR), OLDSTR(I)(1:LOLD(I)))
         IPNT = KPNT + JPNT - 1
         IF (KPNT.GT.0) THEN
            LTEMP = LNEW(I) + LSTR - IPNT - LOLD(I) + 1
            TSTR(1:LTEMP) = NEWSTR(I)(1:LNEW(I)) //
     *         OUTLIN(IPNT+LOLD(I):LSTR)
            OUTLIN(IPNT:IPNT+LTEMP-1) = TSTR(1:LTEMP)
            SCOUNT = SCOUNT + 1
C                                       Check for another occurance
            JPNT =  IPNT + LNEW(I)
            LSTR = LSTR + LNEW(I) - LOLD(I)
            GO TO 50
         ELSE
            JPNT = 1
            END IF
 100        CONTINUE
      IERR = 0
      GO TO 999
C                                       Read error
 500  IERR = 1
      GO TO 999
C                                       EOF
 600  IERR = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A)
      END
      SUBROUTINE AIPCLN (INLINE, LSTR)
C-----------------------------------------------------------------------
C   Routine to do a few AIPSish things
C    Input/output:
C     INLINE  C  Input line
C     LSTR    I  Length of INLINE
C-----------------------------------------------------------------------
      CHARACTER INLINE*(*)
      INTEGER   LSTR
      CHARACTER TEMP*100, BLANK*5
      INTEGER   IPNT, JSTR, ITRIM, NBLANK
      INCLUDE 'XEDIT.INC'
      DATA BLANK /'     '/
C-----------------------------------------------------------------------
C                                       Continuation character = '*'
      IF ((LSTR.GE.6) .AND. (INLINE(1:1).NE.'C') .AND.
     *   (INLINE(6:6).NE.' ')) INLINE(6:6) = '*'
C                                       Minimal indent on cont.
      IF ((LSTR.GE.6) .AND. (INLINE(1:1).NE.'C') .AND.
     *   (INLINE(6:6).EQ.'*') .AND. (INLINE(7:9).NE.'   ')) THEN
         NBLANK = 1
         IF (INLINE(7:8).NE.'  ') NBLANK = 2
         IF (INLINE(7:7).NE.' ') NBLANK = 3
         TEMP(1:LSTR+3) = INLINE(1:6) // BLANK(1:NBLANK) //
     *      INLINE(7:LSTR)
         LSTR = LSTR + NBLANK
         INLINE(1:LSTR) = TEMP(1:LSTR)
         END IF
C                                       Delimiter line
      IF (INLINE(1:30).EQ.CMINUS(1:30)) THEN
         INLINE = CMINUS
         LSTR = 72
         GO TO 999
         END IF
C                                       Beautify DOUBLE PRECISION lines
C                                       from XLATE.
      IF ((INLINE(1:LSTR).EQ.'      DOUBLE PRECISION') .AND.
     *   (LSTR.LE.25)) THEN
C                                       Find non blank line
 20         READ (10,1000,END=999) TEMP
            JSTR = ITRIM (TEMP)
            IF (JSTR.LT.10) GO TO 20
         INLINE(LSTR+1:LSTR+JSTR-8) = ' ' // TEMP(10:JSTR)
         LSTR = LSTR + JSTR - 8
         END IF
C                                       Convert to upper case.
      CALL CHL2U (INLINE, LSTR)
C                                       Add program name to subs list
      IF (INLINE(6:14).EQ.' PROGRAM ') THEN
         NUMSUB = NUMSUB + 1
         OLDSTR(NUMSUB) = '''' // INLINE(15:16) // ''',''' //
     *      INLINE(17:18) // ''',''' // INLINE(19:19) //
     *      ' '''
         NEWSTR(NUMSUB) = '''' // INLINE(15:19) // ' '''
         LOLD(NUMSUB) = 14
         LNEW(NUMSUB) = 8
C                                       Try with blanks between
         NUMSUB = NUMSUB + 1
         OLDSTR(NUMSUB) = '''' // INLINE(15:16) // ''', ''' //
     *      INLINE(17:18) // ''', ''' // INLINE(19:19) // ' '''
         NEWSTR(NUMSUB) = NEWSTR(NUMSUB-1)
         LOLD(NUMSUB) = 16
         LNEW(NUMSUB) = 8
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A80)
      END
      SUBROUTINE LINOUT (OUTLIN, LSTR, MODULE, LMODUL)
C-----------------------------------------------------------------------
C   Subroutine to copy lines longer than 72 characters to MODULE with a
C   terminal '@';  A '@' character in the input line forces a new line
C   with the same indentation as the original.
C    Input:
C     OUTLIN   C  Output line
C     LSTR     I  Number of characters
C     Input/Output:
C     MODULE   C  Module character string
C     LMODUL   I  Next character position in MODULE
C-----------------------------------------------------------------------
      CHARACTER OUTLIN*(*), MODULE*(*)
      INTEGER   LSTR, LMODUL
      CHARACTER CBLANK*40, TSTR*80, BRKCHR*1, TEMP*4096
      LOGICAL   CPRE, CBODY, FORT, BREAK, QUOTE, MATCH
      INTEGER   INDENT, LOOP, IPNT, BRKPNT, LLSTR, LAST, KPNT, ITRIM,
     *   LIMIT, LTEMP, NEXTNQ
      EXTERNAL NEXTNQ
      DATA CBLANK /'C                                       '/
      DATA BRKCHR /'@'/
C-----------------------------------------------------------------------
      IF (LSTR.LE.0) GO TO 999
      LLSTR = LSTR
C                                       Break in line?
      BRKPNT = NEXTNQ (OUTLIN(1:LLSTR), BRKCHR)
      BREAK = BRKPNT.GT.0
C                                       Decide line type: Body comment
      CBODY = OUTLIN(1:40).EQ.CBLANK
      IF (CBODY) INDENT = 40
C                                       Precursor
      CPRE = (.NOT.CBODY) .AND. (OUTLIN(1:1).EQ.'C')
      IF (CPRE) INDENT = 4
C                                       Fortran line
      FORT = OUTLIN(1:1).NE.'C'
C                                       Fortran line indentation
      IF (FORT) THEN
         INDENT = 6
         DO 20 LOOP = 7,LLSTR
            IF (OUTLIN(LOOP:LOOP).NE.' ') GO TO 30
            INDENT = LOOP
 20         CONTINUE
 30      CONTINUE
         END IF
C                                       Check for break in 1st 72 col
 50   IF (BREAK .AND. (BRKPNT.LE.72)) THEN
         MODULE(LMODUL:LMODUL+BRKPNT-2) = OUTLIN(1:BRKPNT-1)
         MODULE(LMODUL+BRKPNT-1:LMODUL+BRKPNT-1) = '@'
         LMODUL = LMODUL + BRKPNT
C                                       Compress line
C                                       Precursor comment
         IF (CPRE) TEMP(1:LLSTR+4) = 'C   ' //OUTLIN(BRKPNT+1:LLSTR)
C                                       Body comment
         IF (CBODY) TEMP(1:LLSTR+40) = CBLANK //OUTLIN(BRKPNT+1:LLSTR)
C                                       Fortran
         IF (FORT) THEN
            TSTR(1:40) = '                                            '
            TEMP(1:LLSTR+INDENT) =
     *         TSTR(1:INDENT) // OUTLIN(BRKPNT+1:LLSTR)
            END IF
         LLSTR = ITRIM (TEMP(1:LLSTR+INDENT))
         OUTLIN(1:LLSTR) = TEMP(1:LLSTR)
C                                       Another break in line?
         BRKPNT = NEXTNQ (OUTLIN(1:LLSTR), BRKCHR)
         BREAK = BRKPNT.GT.0
         GO TO 50
         END IF
C                                       See what's left
 100   IF (LLSTR.LE.72) THEN
         MODULE(LMODUL:LMODUL+LLSTR-1) = OUTLIN(1:LLSTR)
         MODULE(LMODUL+LLSTR:LMODUL+LLSTR) = '@'
         LMODUL = LMODUL + LLSTR + 1
C                                       Done
         GO TO 999
C                                       Long line
       ELSE
C                                       Find last blank or comma, not
C                                       in quotes.
         QUOTE = INDEX(OUTLIN(1:72),'''') .GT. 0
         IF (QUOTE.AND.FORT) THEN
C                                       Find last unmatched quote
C                                       before col 72
            MATCH = .TRUE.
            KPNT = 6
C                                       Don't leave 1 char orphan
            LIMIT = 72
            IF (LLSTR.EQ.73) LIMIT = 71
            DO 200 LOOP = 7,LIMIT
               IF (OUTLIN(LOOP:LOOP).EQ.'''') THEN
                  MATCH = .NOT.MATCH
                  IF (MATCH) KPNT = LOOP
                  END IF
C                                       Last comma OK
               IF (MATCH .AND. (OUTLIN(LOOP:LOOP).EQ.','))  KPNT = LOOP
C                                       Last blank OK
               IF (MATCH .AND. (OUTLIN(LOOP:LOOP).EQ.' '))  KPNT = LOOP
 200           CONTINUE
            IPNT = KPNT
C                                       No quotes
         ELSE
            IPNT = LAST (OUTLIN(1:72), ' ')
            IF (IPNT.LE.INDENT) IPNT = LAST (OUTLIN(1:72), ',')
            IF (IPNT.LE.INDENT) IPNT = 72
            END IF
C                                       Write this line
         MODULE(LMODUL:LMODUL+IPNT-1) = OUTLIN(1:IPNT)
         MODULE(LMODUL+IPNT:LMODUL+IPNT) = '@'
         LMODUL = LMODUL + IPNT + 1
C                                       Deal with rest of line
         LTEMP = LLSTR - IPNT
C                                       Precursor comment
         IF (CPRE) TEMP(1:LTEMP+4) = 'C   ' // OUTLIN(IPNT+1:LLSTR)
C                                       Body comment
         IF (CBODY) TEMP(1:LTEMP+40) = CBLANK // OUTLIN(IPNT+1:LLSTR)
C                                       Fortran continuation
         IF (FORT) THEN
            TSTR = '     *                                        '
            LTEMP = LTEMP + 3
            TEMP(1:LTEMP+INDENT) = TSTR(1:INDENT+3) //
     *          OUTLIN(IPNT+1:LLSTR)
            END IF
         LLSTR = LTEMP + INDENT
         OUTLIN(1:LLSTR) = TEMP(1:LLSTR)
         END IF
C                                       Another break in line?
      BRKPNT = NEXTNQ (OUTLIN(1:LLSTR), BRKCHR)
      BREAK = BRKPNT.GT.0
C                                       Loop back for more
      GO TO 50
C
 999  RETURN
C-----------------------------------------------------------------------
 1250 FORMAT (A)
      END
      SUBROUTINE CALLSQ (OUTLIN, LSTR, NHARG, ARGHOL)
C-----------------------------------------------------------------------
C   Routine to translate call sequences/array arguments.  May force
C   redeclaring a variable as 'HOLLERITH'
C    Input/Output:
C     OUTLIN   C  Output line, more may be read from file.
C     LSTR     I  The number of valid characters in OUTLIN.
C     NHARG    I  Number of elements in ARGHOL
C     ARGHOL   C  List of variables to redeclare as Hollerith
C    From common
C     CCOUNT   I  Count of translations
C     NHKEEP   I  Number of elements in HKEEP
C     HKEEP    C  List of variables to declare if in ARGHOL.
C-----------------------------------------------------------------------
      CHARACTER OUTLIN*(*), ARGHOL(*)*6
      INTEGER   LSTR, NHARG
      CHARACTER STRING*1024, SPARM(100)*50, MPARM(100)*50, FORM*256,
     *   PSTRIN*2, REST*80, XREST*80, HARG*6, ONECH*1, BLANK*80,
     *   TFORM*256
      INTEGER   LOOP, IPTR, JPTR, ICALL, NPARM, SPLEN(100),  INDENT,
     *   MPLEN(100), NMPARM, LENG, PNUM, ITRIM, OPTR, KPTR, LTEMP,
     *   LREST, LX, NCHK, FCHK, SCHK, POINT, SIZE, OLDPNT, IONECH,
     *   PPTR, QPTR, LLTEMP, LSTCAL, LSTPNT, CNTCHA, JCALL
      LOGICAL   T, F, DOREST, NOT, WILD, MATCH, DOMULT, NOCHG
      INCLUDE 'XEDIT.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BLANK /'                                        '/
C-----------------------------------------------------------------------
      NOCHG = .TRUE.
      CNTCHA = 0
      OPTR = 0
      POINT = 7
      OLDPNT = 7
C                                       See if Any routines wanted
      DO 20 LOOP = 1,NUMCAL
         IPTR = INDEX (OUTLIN(POINT:LSTR), OLDCAL(LOOP)(1:LSRC(LOOP)))
         ICALL = LOOP
C                                       Validity check
         PPTR = POINT + IPTR +  LSRC(LOOP) - 1
         ONECH = OUTLIN(PPTR:PPTR)
         IONECH = ICHAR (ONECH)
         IF (((IONECH.GE.ICHA) .AND. (IONECH.LE.ICHZ)) .AND.
     *      (OLDCAL(LOOP)(LSRC(LOOP):LSRC(LOOP)).NE.'(')) GO TO 20
         IF (IPTR.GT.0) GO TO 30
 20      CONTINUE
C                                       None wanted
      GO TO 999
C                                       Remember call and pointer
 30   LSTCAL = ICALL
      LSTPNT = POINT
C                                       Find indentation
      IPTR = IPTR + POINT - 1
      INDENT = IPTR - 1
      POINT = IPTR + LSRC(ICALL)
C                                       Don't loop forever
      CNTCHA = CNTCHA + 1
      IF (CNTCHA.GT.100) GO TO 950
C                                       Parse call sequence - may read
C                                       more from input.
 40   SIZE = LSTR - IPTR + 1
      CALL PARSE (T, OUTLIN(IPTR:LSTR), SIZE, NPARM, SPARM, SPLEN,
     *   LREST, REST)
      LSTR =  SIZE + IPTR - 1
C                                       Parse matching sequence if
C                                       necessary.
      IF (LSRC(ICALL).LT.LOLDC(ICALL)) THEN
         CALL PARSE (F, OLDCAL(ICALL), LOLDC(ICALL), NMPARM, MPARM,
     *      MPLEN, LX, XREST)
C                                       Number of call arguments must
C                                       match.
         IF (NPARM.NE.NMPARM) THEN
            IF (NPARM.GE.NMPARM) WRITE (12,1040) OUTLIN(1:LSTR)
C                                       Save REST if any
            IF (LREST.GT.0) THEN
               OUTLIN(LSTR+1:LSTR+LREST) = REST(1:LREST)
               LSTR = LSTR + LREST
               END IF
            GO TO 900
            END IF
C                                       Check if wanted.
         DO 50 LOOP = 1,NMPARM
C                                       Compare arguments given
            IF (MPLEN(LOOP).GT.0) THEN
C                                       .NOT. argument?
               NOT = MPARM(LOOP)(1:1).EQ.'!'
               FCHK = 1
               IF (NOT) FCHK = 2
C                                       Wild cards?
               WILD = MPARM(LOOP)(MPLEN(LOOP):MPLEN(LOOP)).EQ.'*'
               IF (WILD) THEN
                  NCHK = MPLEN(LOOP) - FCHK
                  SCHK = NCHK
               ELSE
                  NCHK = MPLEN(LOOP) - FCHK + 1
                  SCHK = SPLEN(LOOP)
                  END IF
C                                       Match?
               MATCH = MPARM(LOOP)(FCHK:NCHK+FCHK-1) .EQ.
     *             SPARM(LOOP)(1:SCHK)
               IF (NOT) MATCH = .NOT. MATCH
CC                                       No match for numeric values
C               MATCH = MATCH .AND. ((ICHAR (SPARM(LOOP)(1:1)).GE.ICHA)
C     *            .AND. (ICHAR (SPARM(LOOP)(1:1)).LE.ICHZ))
               IF (.NOT.MATCH) GO TO 900
               END IF
 50         CONTINUE
         END IF
C                                       Save pointer for next search
      OLDPNT = POINT
C                                       Translate
      CCOUNT = CCOUNT + 1
      NOCHG = .FALSE.
C                                       Create new output string
      IPTR = 1
      OPTR = INDENT
      FORM = NEWCAL(ICALL)
      STRING(1:INDENT) = OUTLIN(1:INDENT)
      LENG = LNEWC(ICALL)
C                                       If multiple lines can be written
C                                       and there is text preceeding the
C                                       call assume this is an IF() and
C                                       add a THEN... END IF
      DOMULT = .FALSE.
      LTEMP = INDEX (FORM, '@')
      IF (INDENT.GE.6)
     *   DOMULT = ((STRING(6:INDENT).NE.BLANK(6:INDENT)) .AND.
     *   (LTEMP.GT.0))
      IF (DOMULT) THEN
C                                       Add "THEN@   "
         STRING(INDENT+1:INDENT+8) = 'THEN@   '
         OPTR = OPTR + 8
         END IF
 60   IF (DOMULT) THEN
C                                       Add indentation to format
         TFORM = FORM
         PPTR = 1
         QPTR = 1
 70         LTEMP = INDEX (TFORM(PPTR:),'@')
            IF (LTEMP.GT.0) THEN
               LLTEMP = LTEMP + PPTR - 1
               FORM(QPTR:QPTR+LTEMP+2) = TFORM(PPTR:LLTEMP) // '   '
               QPTR = QPTR + LTEMP + 3
               PPTR = LLTEMP + 1
               LENG = LENG + 3
               GO TO 70
               END IF
C                                       Rest of format
            LLTEMP = LNEWC(ICALL) - PPTR + 1
            IF ((LLTEMP+QPTR-1).GT.250) LLTEMP = 250 - QPTR + 1
            FORM(QPTR:+QPTR+LLTEMP-1) = TFORM(PPTR:PPTR+LLTEMP-1)
         END IF
C                                       Rest of line wanted?
      DOREST = F
      IF (FORM(LENG:LENG).EQ.'%') THEN
         DOREST = T
         LENG = LENG - 1
         END IF
C                                       change it
      DO 100 LOOP = 1,1000
C                                       Find next parameter
         KPTR = INDEX (FORM(IPTR:LENG), '$')
         IF (KPTR.LE.0) GO TO 110
C                                       Parameter number
         PSTRIN = FORM(IPTR+KPTR:IPTR+KPTR+2)
         READ (PSTRIN, 1050) PNUM
C                                       glue line together
         LTEMP = KPTR + SPLEN(PNUM) - 1
         STRING(OPTR+1:OPTR+LTEMP) = FORM(IPTR:IPTR+KPTR-2) //
     *      SPARM(PNUM)(1:SPLEN(PNUM))
         IPTR = IPTR + KPTR + 2
         OPTR = OPTR + KPTR + SPLEN(PNUM) - 1
 100     CONTINUE
C                                       End of line
 110  LTEMP = LENG - IPTR + 1
      IF (IPTR.LE.LENG) THEN
         STRING(OPTR+1:OPTR+LTEMP) = FORM(IPTR:LENG)
         OPTR = OPTR + LTEMP
         END IF
C                                       Prepare to search again
      POINT = OPTR
      INDENT = OPTR
C                                       Rest of input line
      IF (DOREST) THEN
         STRING(OPTR+1:OPTR+LREST+1) = REST(1:LREST)
         OPTR = OPTR + LREST
         END IF
C                                       More?
      IF (OLDCAL(ICALL+1)(1:8).EQ.'continue') THEN
         ICALL = ICALL + 1
         FORM = NEWCAL(ICALL)
         LENG = LNEWC(ICALL)
         IPTR = 1
         GO TO 60
         END IF
C                                       If DOMULT add closing "END IF"
      IF (DOMULT) THEN
         STRING(OPTR+1:OPTR+10) = '@   END IF'
         OPTR = OPTR + 10
         END IF
      OUTLIN(1:OPTR) = STRING(1:OPTR)
      LSTR = OPTR
C                                       Declare HOLLERITH
      IF ((DOHOLL(ICALL)) .AND. (OLDCAL(ICALL)(1:5).NE.'CALL ')) THEN
         HARG = '      '
         DO 180 LOOP = 1,6
            ONECH = NEWCAL(ICALL)(LOOP:LOOP)
            IF ((ONECH.EQ.'(') .OR. (ONECH.EQ.' ') .OR. (ONECH.EQ.','))
     *         GO TO 190
            HARG(LOOP:LOOP) = ONECH
 180        CONTINUE
 190     DO 200 LOOP = 1,NHARG
            IF (HARG.EQ.ARGHOL(LOOP)) GO TO 220
 200        CONTINUE
         NHARG = NHARG + 1
         ARGHOL(NHARG) = HARG
C                                       Log need for EQUIVALENCE
         WRITE(12,1200) HARG
C                                       Add to keep it list
 220     DO 240 LOOP = 1,NHKEEP
            IF (HARG.EQ.HKEEP(LOOP)) GO TO 900
 240        CONTINUE
            NHKEEP = NHKEEP + 1
            HKEEP(NHKEEP) = HARG
            HKDIM(NHKEEP) = HOLDIM(ICALL)
            GO TO 900
         END IF
      GO TO 930
C                                       See if more routines wanted
 900  POINT = 7
      JCALL = ICALL
      DO 920 LOOP = JCALL,NUMCAL
         IPTR = 0
         IF ((LOOP.NE.LSTCAL) .OR. (POINT.GT.LSTPNT))
     *      IPTR = INDEX (OUTLIN(POINT:LSTR),
     *         OLDCAL(LOOP)(1:LSRC(LOOP)))
         ICALL = LOOP
         IF (IPTR.GT.0) THEN
C                                       Validity check
            PPTR = POINT + IPTR +  LSRC(LOOP) - 1
            ONECH = OUTLIN(PPTR:PPTR)
            IONECH = ICHAR (ONECH)
            IF (((IONECH.GE.ICHA) .AND. (IONECH.LE.ICHZ)) .AND.
     *         (OLDCAL(LOOP)(LSRC(LOOP):LSRC(LOOP)).NE.'(')) GO TO 920
            GO TO 30
            END IF
 920     CONTINUE
C                                       Move pointer
      OLDPNT = OLDPNT + LSRC(JCALL)
C                                       Try others
 930  POINT = OLDPNT
      IF (POINT.GE.LSTR) GO TO 999
C                                       Don't bother if haven't found
C                                       anything
      IF (NOCHG) GO TO 999
      NOCHG = .TRUE.
      DO 940 LOOP = 1,NUMCAL
         IPTR = 0
         IF ((LOOP.EQ.LSTCAL) .OR. (POINT.NE.LSTPNT))
     *    IPTR = INDEX (OUTLIN(POINT:LSTR), OLDCAL(LOOP)(1:LSRC(LOOP)))
         ICALL = LOOP
C                                       Validity check
         PPTR = POINT + IPTR +  LSRC(LOOP) - 1
         ONECH = OUTLIN(PPTR:PPTR)
         IONECH = ICHAR (ONECH)
         IF (((IONECH.GE.ICHA) .AND. (IONECH.LE.ICHZ)) .AND.
     *      (OLDCAL(LOOP)(LSRC(LOOP):LSRC(LOOP)).NE.'(')) GO TO 940
         IF (IPTR.GT.0) GO TO 30
 940     CONTINUE
      GO TO 999
C                                       Stuck in a loop
 950  CALL LOGER (OUTLIN, LSTR, POINT, 'CALLSQ: Stuck in line:')
      WRITE (6,1950) OUTLIN(1:70)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT (' Too many arguments       :',A)
 1050 FORMAT (I2)
 1200 FORMAT (' May need to EQUIVALENCE ',A6,' to other data types:')
 1950 FORMAT (' CALLSQ: Got hung up in line:',/1X,A)
      END
      SUBROUTINE PARSE (SOURCE, OUTLIN, LSTR, NPARM, PARM, PLEN,
     *   LREST, REST)
C-----------------------------------------------------------------------
C   Parse a call sequence, may read more lines to complete line.
C   On output OUTLIN contains all that has been read.
C    Input:
C     SOURCE   L  True if from source file, else template.
C    Input/Output:
C     OUTLIN   C  Output line, more may be read from file.
C     LSTR     I  The number of valid characters in OUTLIN.
C    Output:
C     NPARM    I  Number of parameters found
C     PARM(*)  C  Parameters (blanks are removed).
C     PLEN(*)  I  Numbers of characters in the elements of PARM.
C     LREST    I  Number of characters in REST
C     REST     C  Line after portion parsed.
C-----------------------------------------------------------------------
      LOGICAL   SOURCE
      CHARACTER OUTLIN*(*), PARM(*)*50, REST*(*)
      INTEGER   LSTR, NPARM, PLEN(*), LREST
      CHARACTER STRING*1024, LINE*128, TSTR*50, TSTR2*50
      INTEGER   END, IPNT, PLEVEL, LOOP, LENG, JPNT, LOOP2, ITRIM, IPTR,
     *   IP, LTEMP, IERR, LPNT
      LOGICAL   PARTL, START
C-----------------------------------------------------------------------
      STRING = OUTLIN(1:LSTR)
      END = LSTR
      NPARM = 1
      PLEN(NPARM) = 0
      PARM(NPARM) = ' '
      PARTL = .TRUE.
      START = .FALSE.
      JPNT = 0
      LREST = 0
C                                       Start at first '('
 50   IPNT = INDEX (STRING(1:END), '(')
      IF (IPNT.LE.0) GO TO 999
C                                       Don't if "," first
      LPNT = INDEX (STRING(1:END), ',')
      IF ((LPNT.GT.0) .AND. (LPNT.LT.IPNT)) GO TO 999
C                                       Need another line?
      IF ((IPNT.LE.0) .AND. SOURCE) THEN
         IPNT = END + 1
         GO TO 290
         END IF
      IF (IPNT.LE.0) GO TO 500
      START = .TRUE.
      IPNT = IPNT + 1
      PLEVEL = 1
C                                       Find next level 1 ','
 100  PLEN(NPARM) = 0
      PARM(NPARM) = ' '
 110  PARTL = .FALSE.
      DO 200 LOOP = IPNT,END
         JPNT = LOOP
         IF (STRING(LOOP:LOOP).EQ.'(') PLEVEL = PLEVEL + 1
         IF (STRING(LOOP:LOOP).EQ.')') PLEVEL = PLEVEL - 1
C                                       Done?
         IF (PLEVEL.LE.0) GO TO 250
         IF ((STRING(LOOP:LOOP).EQ.',') .AND. (PLEVEL.EQ.1)) GO TO 250
 200     CONTINUE
C                                       Parameter not done
      PARTL = .TRUE.
C                                       Save parameter, partial
 250  IF (PARTL) THEN
         IP = PLEN(NPARM) + 1
         LTEMP = JPNT - IPNT + 1
         PARM(NPARM)(IP:IP+LTEMP) = STRING(IPNT:JPNT)
         PLEN(NPARM) = PLEN(NPARM) + JPNT - IPNT + 1
C                                       Full parameter
      ELSE
         IP = PLEN(NPARM) + 1
         LTEMP = JPNT - IPNT
         PARM(NPARM)(IP:IP+LTEMP) = STRING(IPNT:JPNT-1)
         PLEN(NPARM) = PLEN(NPARM) + JPNT - IPNT
         NPARM = NPARM + 1
         END IF
      IPNT = JPNT + 1
C                                       Done?
      IF (PLEVEL.LE.0) GO TO 500
C                                       Read another line if necessary
 290   IF (SOURCE .AND. (IPNT.GT.END)) THEN
         CALL GETLIN (STRING, END, IERR)
         IF (IERR.NE.0) GO TO 950
C                                       Better be a continuation line
         IF (STRING(6:6).EQ.' ') GO TO 700
C                                       Find first non blank
         DO 300 LOOP = 7,END
            IF (STRING(LOOP:LOOP).NE.' ') THEN
               IPNT = LOOP
               GO TO 310
               END IF
 300        CONTINUE
C                                       Glue new line to OUTLIN
 310     LTEMP = END - IPNT + 2
         OUTLIN(LSTR+1:LSTR+LTEMP) = ' ' // STRING(IPNT:END)
         LSTR = LSTR + END - IPNT + 2
         END IF
C                                       Begining "(" found yet?
      IF (.NOT.START) GO TO 50
      IF ((.NOT.SOURCE) .AND. (IPNT.GT.END)) GO TO 500
C                                       Next parameter
      IF (PARTL) GO TO 110
      IF (.NOT.PARTL) GO TO 100
C                                       Get rest of line
 500  LREST = END - JPNT
      REST(1:LREST) = STRING(JPNT+1:END)
C                                       Compress parameters
      NPARM = NPARM - 1
      DO 600 LOOP = 1,NPARM
         LENG = PLEN(LOOP)
         PLEN(LOOP) = 0
         TSTR = PARM(LOOP)
         TSTR2 = ' '
         IPTR = 0
         DO 550 LOOP2 = 1,LENG
            IF (TSTR(LOOP2:LOOP2).NE.' ') THEN
               IPTR = IPTR + 1
               TSTR2(IPTR:IPTR) = TSTR(LOOP2:LOOP2)
               END IF
 550        CONTINUE
         PARM(LOOP) = TSTR2(1:IPTR)
         PLEN(LOOP) = IPTR
 600     CONTINUE
      GO TO 999
C                                       Problem - not cont. line
 700  WRITE (6,1700) STRING(1:END)
      WRITE (12,1700) STRING(1:END)
C                                       Save rest of line
      REST(LREST+1:LREST+END) = '@' // STRING(7:END)
      LREST = LREST + END - 5
      IF (STRING(1:6).NE.'      ') THEN
         WRITE(6,1701)
         WRITE(12,1701)
         END IF
      NPARM = 0
      GO TO 999
C                                       End of file or error
 950  WRITE (6,1950)
C
 999  RETURN
C-----------------------------------------------------------------------
 1250 FORMAT (A)
 1700 FORMAT (' PARSE: Continuation line expected, found:',/1X,A)
 1701 FORMAT (' PARSE: Label or Comment delimiter lost from line!!!')
 1950 FORMAT (' PARSE: Trouble reading input Fortran file')
      END
      SUBROUTINE LOGIT (PAGE, MODULE, LMODUL, NUMLOG, LOGSTR, LLOG,
     *   LOGREA)
C-----------------------------------------------------------------------
C   Subroutine to enter occurances of the logging strings in the .LOG
C    Inputs:
C     PAGE     I  Number of pages in MODULE
C     MODULE   C  First page of the Module buffer.
C     LMODUL   I  Number of characters in MODULE.
C     NUMLOG   I  Number of elements in LOGSTR
C     LOGSTR(*)C  Strings to log
C     LLOG(*)  I  Length of strings in LOGSTR
C     LOGREA(*)C  Reason strings
C-----------------------------------------------------------------------
      INTEGER   PAGE
      CHARACTER MODULE(PAGE)*(*), LOGSTR(*)*(*), LOGREA(*)*25
      INTEGER   LMODUL(PAGE), LLOG(*), NUMLOG
      CHARACTER ARGSTR*10
      INTEGER   FIRST, LOOP, IPAGE, ILOG, BACK, POINT, IPNT, ITRIM,
     *   LARG, LINEND(2,20), FRONT, END, LAST, HERE, BCKCHK, IUSE,
     *   EEND, N72
      DATA N72 /72/
C-----------------------------------------------------------------------
      IF (NUMLOG.LE.0) GO TO 999
      FIRST = 1
C                                       Loop over pages
      DO 500 IPAGE = 1,PAGE
C                                       Loop over arguments
         DO 400 ILOG = 1,NUMLOG
            POINT = FIRST
C                                       Find occurance
            LARG = LLOG(ILOG)
            IF (LARG.LE.0) GO TO 400
 100           IPNT = INDEX (MODULE(IPAGE)(POINT:LMODUL(IPAGE)),
     *            LOGSTR(ILOG)(1:LARG))
               IF (IPNT.LE.0) GO TO 400
               HERE = POINT + IPNT - 1
               END = INDEX (MODULE(IPAGE)(HERE:HERE+75), '@') +
     *            HERE - 1
C                                       Reset POINT for next search
               POINT = END + 1
C                                       Log it
               CALL LOGER (MODULE(IPAGE), LMODUL(IPAGE), HERE,
     *            LOGREA(ILOG))
 300           GO TO 100
 400        CONTINUE
         FIRST = 1
 500     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE LOGER (MODULE, LMODUL, HERE, LABEL)
C-----------------------------------------------------------------------
C   Subroutine to add anotated lines of source code to the log file.
C    Inputs:
C     MODULE   C  Current page of the Module buffer.
C     LMODUL   I  Number of characters in MODULE.
C     HERE     I  Pointer in statment to be printed
C     LABEL    C  Label for first line of statment
C-----------------------------------------------------------------------
      CHARACTER MODULE*(*), LABEL*(*)
      INTEGER   LMODUL
      CHARACTER BLANK*50
      INTEGER   FIRST, LOOP, BACK, IPNT, LINEND(2,20), FRONT, END,
     *   LAST, HERE, BCKCHK, EEND, LABLEN, IFST, IEND,
     *   N72
      EXTERNAL LAST
      DATA N72 /72/
      DATA BLANK /'                                                  '/
C-----------------------------------------------------------------------
      LABLEN = LEN (LABEL)
C                                       Find end of line
      BACK = 0
      END = INDEX (MODULE(HERE:HERE+300), '@') + HERE - 1
      EEND = END
      BCKCHK = MIN (N72, HERE) - 1
      FRONT = LAST (MODULE(HERE-BCKCHK:HERE), '@') + HERE - BCKCHK - 1
C                                       Backup to first line of
C                                       continuation series.
C                                       Find end
 150     BCKCHK = MIN (N72, HERE) - 1
         FRONT = LAST (MODULE(HERE-BCKCHK:HERE), '@') +
     *      HERE - BCKCHK - 1
C                                       Save ends
         BACK = BACK + 1
         LINEND(1,BACK) = FRONT
         LINEND(2,BACK) = END
         HERE = FRONT - 2
         END = FRONT
C                                       Continuation line? loop
         IF ((MODULE(FRONT+6:FRONT+6).NE.' ') .AND.
     *      (HERE.GT.0)) GO TO 150
C                                       FORMAT?
      IF (MODULE(FRONT+7:FRONT+12).EQ.'FORMAT')
     *   GO TO 999
C                                       Log it.
      DO 250 LOOP = 1,BACK
         IPNT = BACK - LOOP + 1
         FRONT = LINEND(1,IPNT) + 1
         END = LINEND(2,IPNT) - 1
         IEND = END
         IF (IEND.GT.(FRONT+99)) IEND = FRONT + 99
         IF (LOOP.EQ.1) THEN
            WRITE (12,1200,ERR=999) LABEL(1:LABLEN), MODULE(FRONT:IEND)
         ELSE
            WRITE (12,1200,ERR=999) BLANK(1:LABLEN), MODULE(FRONT:IEND)
            END IF
C                                       Do rest of line
 230     IF (IEND.LT.END) THEN
            FRONT = IEND + 1
            IEND = END
            IF (IEND.GT.(FRONT+99)) IEND = FRONT + 99
            WRITE (12,1200,ERR=999) BLANK(1:LABLEN), MODULE(FRONT:IEND)
            GO TO 230
            END IF
 250     CONTINUE
C                                       Do any more continuation lines
 260  IF (END .GE. LMODUL) GO TO 999
      IF ((MODULE(END+2:END+2).NE.'C') .AND.
     *   (MODULE(END+7:END+7).NE.' ')) THEN
         FRONT = END + 2
         END = INDEX (MODULE(FRONT:FRONT+75), '@')
     *      + FRONT - 2
         IEND = END
         IF (IEND.GT.(FRONT+99)) IEND = FRONT + 99
         WRITE (12,1200,ERR=999) BLANK(1:LABLEN), MODULE(FRONT:IEND)
C                                       Do rest of line
 270     IF (IEND.LT.END) THEN
            FRONT = IEND + 1
            IEND = END
            IF (IEND.GT.(FRONT+99)) IEND = FRONT + 99
            WRITE (12,1200,ERR=999) BLANK(1:LABLEN), MODULE(FRONT:IEND)
            GO TO 270
            END IF
         GO TO 260
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT (1X,A,': ',A)
      END
      SUBROUTINE PRTMOD (PAGE, MODULE, LMODUL, LCOUNT)
C-----------------------------------------------------------------------
C   Routine to write the contents of MODULE on unit 11.  A line break is
C   inserted every '@' character.  Blank lines are removed.
C    Inputs:
C     PAGE       I  Number of pages used in MODULE
C     MODULE(*)  C  Module string (65 kbytes)
C     LMODUL(*)  I  Number of valid characters in MODULE.
C   Input/Output:
C     LCOUNT     I  Line count
C-----------------------------------------------------------------------
      INTEGER   PAGE
      CHARACTER MODULE(PAGE)*(*)
      INTEGER   LMODUL(PAGE), LCOUNT
      INTEGER   START, END, LOOP, IEND, ITRIM, NEXTNQ
      EXTERNAL  ITRIM, NEXTNQ
C-----------------------------------------------------------------------
C                                       Loop over page
      DO 200 LOOP = 1,PAGE
         IF (LMODUL(LOOP).LE.0) GO TO 999
         START = 1
C                                       Loop printing
 100        END = NEXTNQ (MODULE(LOOP)(START:START+200), '@')
C                                       Trap '@' problems
            IF ((END.LE.0) .OR. (END.GT.73)) THEN
C                                       Break at next '@'
               END = INDEX (MODULE(LOOP)(START:START+1000), '@')
               IF (END.LE.0) GO TO 990
               END IF
            END = END + START - 2
            IEND = ITRIM (MODULE(LOOP)(START:END)) + START - 1
            IF (IEND.GE.START) THEN
               WRITE (11,1100,ERR=990) MODULE(LOOP)(START:IEND)
               LCOUNT = LCOUNT + 1
               END IF
            START = END + 2
            IF (START.LT.LMODUL(LOOP)) GO TO 100
 200     CONTINUE
      GO TO 999
C                                       problem
 990  WRITE (6,1990)
      WRITE (12,1990)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A)
 1990 FORMAT (' ******* PRTMOD failed; module partially written.')
      END
      SUBROUTINE DCLEDT (PAGE, MODULE, LMODUL, NHARG, ARGHOL)
C-----------------------------------------------------------------------
C   Routine to redeclare Hollerith and Character variables.
C   See SETUP for info in common.
C   Assumes enough room left on  page 1 for the additional declarations.
C    Input/Output:
C     PAGE    I  Number of pages in MODULE
C     MODULE  C  Module string, '@' => line break
C     LMODUL  I  Number of valid characters in MODULE (including '@')
C     NHARG   I  Number of elements in ARGHOL
C     ARGHOL  C  List of variables to redeclare as Hollerith
C   Input from common
C     BEGDCL       I  Pointer to the first character of the declaration
C     ENDDCL       I  Pointer to the last character of the declaration
C     BEGFOR       I  Pointer to the first character of the executable
C     DCLPOS(3,*)  I  Declaration position pointers wrt BEGDCL
C                     First character, last character, type:
C                     1 = INTEGER, 2 = REAL, 3 = HOLLERITH,
C                     4 = CHARACTER, 5 = LOGICAL, 6 = DOUBLE PRECISION,
C                     7 = COMPLEX
C     NUMDCL       I  Number of entries in DCLPOS
C-----------------------------------------------------------------------
      INTEGER   PAGE
      CHARACTER MODULE(PAGE)*(*)
      INTEGER   LMODUL(PAGE)
      CHARACTER STRING*32767
      CHARACTER DCL*4096, ARGHOL(500)*6, ARGCHR(500)*6, HOLUSE(50)*6,
     *   CHRUSE(50)*6, CHRDIM(500)*20
      INTEGER   NHARG, NCARG, NHUSE, NCUSE, SIZE, LDCL, LSTR, NADD
      LOGICAL   T, F
      INCLUDE 'XEDIT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF ((NUMHOL.LE.0) .AND. (NUMCHR.LE.0)) GO TO 999
      LDCL = 0
      NCARG = 0
      IF (BEGDCL.GE.ENDDCL) GO TO 999
C                                       Find Character calls
      IF (NUMCHR.GT.0) THEN
         CALL FNDVAR (T, PAGE, MODULE, LMODUL, BEGFOR, NUMCHR, CHRDCL,
     *      NHARG, ARGHOL, CHRARG(1,1,1), CHRARG(1,1,2), NCARG, NCUSE,
     *      ARGCHR, CHRUSE, CHRDIM)
C                                       Find CHARACTERs from DATA
C                                       statments
         CALL DATCHR (MODULE(1), LMODUL(1), BEGFOR, BEGDCL, DCLPOS,
     *      NUMDCL, NCARG, ARGCHR, CHRDIM)
C                                       Find forced declarations
         CALL FNDUSE (PAGE, MODULE, LMODUL, BEGFOR, NCKEEP, CKEEP,
     *      NCARG, ARGCHR)
C                                       Form Character declarations
         SIZE = ENDDCL - BEGDCL + 1
         CALL CHRDL (MODULE(1)(BEGDCL:ENDDCL), SIZE, DCLPOS, NUMDCL,
     *      NUMCHR, CHRARG, NCARG, ARGCHR, CHRDIM, NCKEEP,
     *      CKEEP, CKDIM, MODULE(1)(1:BEGDCL), DCL, LDCL)
C                                       Log warnings about other uses
C                                       Add 'WRITE' to list of allowed
C                                       uses
         NCUSE = NCUSE + 1
         CHRUSE(NCUSE) = 'WRITE'
         CALL DCLWRN (PAGE, ENDDCL, MODULE, LMODUL, NCARG, NCUSE,
     *      ARGCHR, CHRUSE, 'CHARACTER ')
         END IF
C                                       Find Hollerith calls
      IF (NUMHOL.GT.0) THEN
         CALL FNDVAR (F, PAGE, MODULE, LMODUL, BEGFOR, NUMHOL, HOLDCL,
     *      NCARG, ARGCHR, HOLARG, HOLARG, NHARG, NHUSE, ARGHOL,
     *      HOLUSE, CHRDIM)
C                                       Find forced declarations
         CALL FNDUSE (PAGE, MODULE, LMODUL, BEGFOR, NHKEEP, HKEEP,
     *      NHARG, ARGHOL)
C                                       Form Hollerith declarations
         SIZE = ENDDCL - BEGDCL + 1
         CALL HOLDL (MODULE(1)(BEGDCL:ENDDCL), SIZE, DCLPOS, NUMDCL,
     *      NHARG, ARGHOL, NHKEEP, HKEEP, HKDIM, MODULE(1)(1:BEGDCL),
     *      DCL, LDCL)
C                                       Log warnings about other uses
C                                       uses
         CALL DCLWRN (PAGE, ENDDCL, MODULE, LMODUL, NHARG, NHUSE,
     *      ARGHOL, HOLUSE, 'HOLLERITH ')
         END IF
C                                       Make sure '@' at beginning of
C                                       declarations is there.
      MODULE(1)(BEGDCL:BEGDCL) = '@'
      IF ((NHARG.GT.0) .OR. (NCARG.GT.0)) THEN
C                                       Copy precursor
         STRING(1:BEGDCL) = MODULE(1)(1:BEGDCL)
         LSTR = BEGDCL + 1
C                                       Format declarations
         CALL LINOUT (DCL, LDCL, STRING, LSTR)
         NADD = LSTR - BEGDCL - 1
C                                       Copy rest of module
         SIZE = LMODUL(1) - BEGDCL
         STRING(LSTR:LSTR+SIZE-1) = MODULE(1)(BEGDCL+1:LMODUL(1))
         LSTR = LSTR + SIZE - 1
C                                       Copy back
         MODULE(1)(1:LSTR) = STRING(1:LSTR)
         LMODUL(1) = LSTR
C                                       Update pointers
         BEGDCL = BEGDCL + NADD
         ENDDCL = ENDDCL + NADD
         BEGFOR = BEGFOR + NADD
         END IF
 999  RETURN
      END
      SUBROUTINE PARSEC (STRING, NPARM, PARM, PLEN)
C-----------------------------------------------------------------------
C   Parse a call sequence from a character array.
C   '@' is assumed to the the end of line character.
C    Input
C     STRING   C  String to parse.
C    Output:
C     NPARM    I  Number of parameters found
C     PARM(*)  C  Parameters (blanks and subscripts are removed).
C                 Should be 50 characters.
C     PLEN(*)  I  Numbers of characters in the elements of PARM.
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), PARM(*)*50
      INTEGER   NPARM, PLEN(*)
      CHARACTER WORK*80, TSTR*50, TSTR2*50
      INTEGER   END, IPNT, PLEVEL, LOOP, LENG, JPNT, LOOP2, ITRIM,
     *   POINT, IPTR, IP, LTEMP
      LOGICAL   PARTL
C-----------------------------------------------------------------------
      POINT = 1
C                                       Extract rest of current line
      IPNT = INDEX (STRING(POINT:POINT+75), '@')
      WORK = STRING(POINT:POINT+IPNT-2)
      END = IPNT
      POINT = POINT + IPNT
C                                       Start at first '('
      IPNT = INDEX (WORK(1:END), '(')
      NPARM = 1
      IF ((WORK(1:1).EQ.',') .OR. (WORK(1:2).EQ.' ,')) GO TO 500
      IF (IPNT.LE.0) GO TO 500
      PLEVEL = 1
      IPNT = IPNT + 1
C                                       Find next level 1 ','
 100  PLEN(NPARM) = 0
      PARM(NPARM) = ' '
 110  PARTL = .FALSE.
      DO 200 LOOP = IPNT,END
         JPNT = LOOP
         IF (WORK(LOOP:LOOP).EQ.'(') PLEVEL = PLEVEL + 1
         IF (WORK(LOOP:LOOP).EQ.')') PLEVEL = PLEVEL - 1
C                                       Done?
         IF (PLEVEL.LE.0) GO TO 250
         IF ((WORK(LOOP:LOOP).EQ.',') .AND. (PLEVEL.EQ.1)) GO TO 250
 200     CONTINUE
      JPNT = END
C                                       Parameter not done
      PARTL = .TRUE.
C                                       Save parameter, partial
 250  IF (PARTL) THEN
         IP = PLEN(NPARM) + 1
         LTEMP = JPNT - IPNT
         PARM(NPARM)(IP:IP+LTEMP) = WORK(IPNT:JPNT)
         PLEN(NPARM) = PLEN(NPARM) + JPNT - IPNT + 1
C                                       Full parameter
      ELSE
         IP = PLEN(NPARM) + 1
         LTEMP = JPNT - IPNT - 1
         PARM(NPARM)(IP:IP+LTEMP) = WORK(IPNT:JPNT-1)
         PLEN(NPARM) = PLEN(NPARM) + JPNT - IPNT
         NPARM = NPARM + 1
         END IF
      IPNT = JPNT + 1
C                                       Done?
      IF (PLEVEL.LE.0) GO TO 500
C                                       Fetch another line if necessary
      IF (JPNT.GE.END) THEN
         IPNT = INDEX (STRING(POINT+1:POINT+75), '@') + 1
         WORK = STRING(POINT+1:POINT+IPNT-2)
         END = IPNT
         POINT = POINT + IPNT
         IPNT = 7
         END IF
C                                       Next parameter
      IF (PARTL) GO TO 110
      IF (.NOT.PARTL) GO TO 100
C                                       Compress parameters, remove
C                                       parenthese
 500  NPARM = NPARM - 1
      DO 600 LOOP = 1,NPARM
         LENG = PLEN(LOOP)
         PLEN(LOOP) = 0
         TSTR = PARM(LOOP)
         TSTR2 = ' '
         IPTR = 0
         DO 550 LOOP2 = 1,LENG
            IF (TSTR(LOOP2:LOOP2).EQ.'(') GO TO 560
            IF (TSTR(LOOP2:LOOP2).NE.' ') THEN
               IPTR = IPTR + 1
               TSTR2(IPTR:IPTR) = TSTR(LOOP2:LOOP2)
               END IF
 550        CONTINUE
 560     PARM(LOOP) = TSTR2(1:IPTR)
         PLEN(LOOP) = IPTR
 600     CONTINUE
      GO TO 999
C                                       End of file or error
 950  WRITE (6,1950)
C
 999  RETURN
C-----------------------------------------------------------------------
 1250 FORMAT (A)
 1950 FORMAT ('PARSEC: Trouble reading input Fortran file')
      END
      SUBROUTINE FNDDCL (MODULE, LMODUL)
C-----------------------------------------------------------------------
C   Subroutine to find the beginning and end of the declarations and
C   the first line of the executable (NOT DATA).
C    Inputs:
C     MODULE   C  First page of the Module buffer.
C     LMODUL   I  Number of characters in MODULE.
C   Outputs in common:
C     BEGDCL       I  Pointer to the first character of the declaration
C     ENDDCL       I  Pointer to the last character of the declaration
C     BEGFOR       I  Pointer to the first character of the executable
C     DCLPOS(3,*)  I  Declaration position pointers wrt BEGDCL
C                     First character, last character, type:
C                     1 = INTEGER, 2 = REAL, 3 = HOLLERITH,
C                     4 = CHARACTER, 5 = LOGICAL, 6 = DOUBLE PRECISION,
C                     7 = COMPLEX
C     NUMDCL       I  Number of entries in DCLPOS
C-----------------------------------------------------------------------
      CHARACTER MODULE*(*)
      INTEGER   LMODUL
      INTEGER   INT1, REA1, HOL1, CHR1, LOG1, DP1, CPX1, DAT1,
     *   INTEND, REAEND, HOLEND, CHREND, LOGEND, DPEND, CPXEND, DATEND,
     *   POINT, NEXT, PCALL, PEQUAL, LAST, LOOP
      CHARACTER*15 INT, REA, HOL, CHR, LOG, DP, CPX
      CHARACTER DAT*12
      INCLUDE 'XEDIT.INC'
      DATA       INT,             REA,                 HOL
     *   /'@      INTEGER ', '@      REAL    ', '@      HOLLERIT'/
      DATA       CHR,             LOG,                DP
     *   /'@      CHARACTE', '@      LOGICAL ', '@      DOUBLE P'/
      DATA       CPX,              DAT
     *   /'@      COMPLEX ', '@      DATA '/
C-----------------------------------------------------------------------
      NUMDCL = 0
C                                       Find first, last INTEGER
      INT1 = 99999
      INTEND = 0
      POINT = INDEX (MODULE(1:LMODUL), INT)
      IF (POINT.GT.0) THEN
         NEXT = 0
         INT1 = POINT
C                                       Find last INTEGER
 10         POINT = POINT + NEXT + 15
            NUMDCL = NUMDCL + 1
            DCLPOS(1,NUMDCL) = POINT - 15
            DCLPOS(3,NUMDCL) = 1
C                                       Find end of last cont. card
 20            NEXT = INDEX (MODULE(POINT+1:POINT+75), '@') + 1
               POINT = POINT + NEXT - 1
               INTEND = POINT
               DCLPOS(2,NUMDCL) = POINT
               IF (((POINT+1).LT.LMODUL) .AND.
     *             (MODULE(POINT+6:POINT+6).NE.' ') .AND.
     *             (MODULE(POINT+1:POINT+1).NE.'C')) GO TO 20
            NEXT = INDEX (MODULE(POINT:LMODUL), INT)
            IF (NEXT.GT.0) GO TO 10
         END IF
C                                       Find first, last REAL
      REA1 = 99999
      REAEND = 0
      POINT = INDEX (MODULE(1:LMODUL), REA(1:12))
      IF (POINT.GT.0) THEN
         NEXT = 0
         REA1 = POINT
C                                       Find last REAL
 30         POINT = POINT + NEXT + 12
            NUMDCL = NUMDCL + 1
            DCLPOS(1,NUMDCL) = POINT - 12
            DCLPOS(3,NUMDCL) = 2
C                                       Find end of last cont. card
 40            NEXT = INDEX (MODULE(POINT+1:POINT+75), '@') + 1
               POINT = POINT + NEXT - 1
               REAEND = POINT
               DCLPOS(2,NUMDCL) = POINT
               IF (((POINT+1).LT.LMODUL) .AND.
     *             (MODULE(POINT+6:POINT+6).NE.' ') .AND.
     *             (MODULE(POINT+1:POINT+1).NE.'C')) GO TO 40
            NEXT = INDEX (MODULE(POINT:LMODUL), REA(1:12))
            IF (NEXT.GT.0) GO TO 30
         END IF
C                                       Find first, last HOLLERITH
      HOL1 = 99999
      HOLEND = 0
      POINT = INDEX (MODULE(1:LMODUL), HOL)
      IF (POINT.GT.0) THEN
         NEXT = 0
         HOL1 = POINT
C                                       Find last HOLLERITH
 50         POINT = POINT + NEXT + 15
            NUMDCL = NUMDCL + 1
            DCLPOS(1,NUMDCL) = POINT - 15
            DCLPOS(3,NUMDCL) = 3
C                                       Find end of last cont. card
 60            NEXT = INDEX (MODULE(POINT+1:POINT+75), '@') + 1
               POINT = POINT + NEXT - 1
               DCLPOS(2,NUMDCL) = POINT
               HOLEND = POINT
               IF (((POINT+1).LT.LMODUL) .AND.
     *             (MODULE(POINT+6:POINT+6).NE.' ') .AND.
     *             (MODULE(POINT+1:POINT+1).NE.'C')) GO TO 60
            NEXT = INDEX (MODULE(POINT:LMODUL), HOL)
            IF (NEXT.GT.0) GO TO 50
         END IF
C                                       Find first, last CHARACTER
      CHR1 = 99999
      CHREND = 0
      POINT = INDEX (MODULE(1:LMODUL), CHR)
      IF (POINT.GT.0) THEN
         NEXT = 0
         CHR1 = POINT
C                                       Find last CHARACTER
 70         POINT = POINT + NEXT + 15
            NUMDCL = NUMDCL + 1
            DCLPOS(1,NUMDCL) = POINT - 15
            DCLPOS(3,NUMDCL) = 4
C                                       Find end of last cont. card
 80            NEXT = INDEX (MODULE(POINT+1:POINT+75), '@') + 1
               POINT = POINT + NEXT - 1
               DCLPOS(2,NUMDCL) = POINT
               CHREND = POINT
               IF (((POINT+1).LT.LMODUL) .AND.
     *             (MODULE(POINT+6:POINT+6).NE.' ') .AND.
     *             (MODULE(POINT+1:POINT+1).NE.'C')) GO TO 80
            NEXT = INDEX (MODULE(POINT:LMODUL), CHR)
            IF (NEXT.GT.0) GO TO 70
         END IF
C                                       Find first, last LOGICAL
      LOG1 = 99999
      LOGEND = 0
      POINT = INDEX (MODULE(1:LMODUL), LOG)
      IF (POINT.GT.0) THEN
         NEXT = 0
         LOG1 = POINT
C                                       Find last LOGICAL
 90         POINT = POINT + NEXT + 15
            NUMDCL = NUMDCL + 1
            DCLPOS(1,NUMDCL) = POINT - 15
            DCLPOS(3,NUMDCL) = 5
C                                       Find end of last cont. card
 100           NEXT = INDEX (MODULE(POINT+1:POINT+75), '@') + 1
               POINT = POINT + NEXT - 1
               DCLPOS(2,NUMDCL) = POINT
               LOGEND = POINT
               IF (((POINT+1).LT.LMODUL) .AND.
     *             (MODULE(POINT+6:POINT+6).NE.' ') .AND.
     *             (MODULE(POINT+1:POINT+1).NE.'C')) GO TO 100
            NEXT = INDEX (MODULE(POINT:LMODUL), LOG)
            IF (NEXT.GT.0) GO TO 90
         END IF
C                                       Find first,last DOUBLE PRECISION
      DP1 = 99999
      DPEND = 0
      POINT = INDEX (MODULE(1:LMODUL), DP)
      IF (POINT.GT.0) THEN
         NEXT = 0
         DP1 = POINT
C                                       Find last DOUBLE PRECISION
 110        POINT = POINT + NEXT + 15
            NUMDCL = NUMDCL + 1
            DCLPOS(1,NUMDCL) = POINT - 15
            DCLPOS(3,NUMDCL) = 6
C                                       Find end of last cont. card
 120           NEXT = INDEX (MODULE(POINT+1:POINT+75), '@') + 1
               POINT = POINT + NEXT - 1
               DPEND = POINT
               DCLPOS(2,NUMDCL) = POINT
               IF (((POINT+1).LT.LMODUL) .AND.
     *             (MODULE(POINT+6:POINT+6).NE.' ') .AND.
     *             (MODULE(POINT+1:POINT+1).NE.'C')) GO TO 120
            NEXT = INDEX (MODULE(POINT:LMODUL), DP)
            IF (NEXT.GT.0) GO TO 110
         END IF
C                                       Find first, last COMPLEX
      CPX1 = 99999
      CPXEND = 0
      POINT = INDEX (MODULE(1:LMODUL), CPX)
      IF (POINT.GT.0) THEN
         NEXT = 0
         CPX1 = POINT
C                                       Find last COMPLEX
 130        POINT = POINT + NEXT + 15
            NUMDCL = NUMDCL + 1
            DCLPOS(1,NUMDCL) = POINT - 15
            DCLPOS(3,NUMDCL) = 7
C                                       Find end of last cont. card
 140           NEXT = INDEX (MODULE(POINT+1:POINT+75), '@') + 1
               POINT = POINT + NEXT - 1
               CPXEND = POINT
               DCLPOS(2,NUMDCL) = POINT
               IF (((POINT+1).LT.LMODUL) .AND.
     *             (MODULE(POINT+6:POINT+6).NE.' ') .AND.
     *             (MODULE(POINT+1:POINT+1).NE.'C')) GO TO 140
            NEXT = INDEX (MODULE(POINT:LMODUL), CPX)
            IF (NEXT.GT.0) GO TO 130
         END IF
C                                       Set beginning, end of
C                                       declarations
      BEGDCL = MIN (INT1, REA1, HOL1, CHR1, LOG1, DP1, CPX1)
      ENDDCL = MAX (INTEND, REAEND, HOLEND, CHREND, LOGEND, DPEND,
     *   CPXEND)
C                                       Refer DCLPOS to BEGDCL
      DO 150 LOOP = 1,NUMDCL
         DCLPOS(1,LOOP) = DCLPOS(1,LOOP) - BEGDCL + 1
         DCLPOS(2,LOOP) = DCLPOS(2,LOOP) - BEGDCL + 1
 150     CONTINUE
C                                       Find beginning of CODE
C                                       i.e. End of DATA
      DATEND = ENDDCL
      POINT = INDEX (MODULE(1:LMODUL), DAT)
      BEGFOR = ENDDCL + 1
      IF (POINT.GT.0) THEN
         NEXT = 0
C                                       Find last DATA
 200        POINT = POINT + NEXT + 15
            NEXT = INDEX (MODULE(POINT:LMODUL), DAT)
            IF (NEXT.GT.0) GO TO 200
C                                       Find end of last cont. card
 210        NEXT = INDEX (MODULE(POINT+1:POINT+75), '@') + 1
            POINT = POINT + NEXT - 1
            DATEND = POINT
            IF (((POINT+1).LT.LMODUL) .AND.
     *         (MODULE(POINT+6:POINT+6).NE.' ') .AND.
     *         (MODULE(POINT+1:POINT+1).NE.'C')) GO TO 210
         BEGFOR = DATEND + 1
C                                       If no DATA use first call or =
      ELSE
         PCALL = INDEX (MODULE(BEGFOR:LMODUL), ' CALL ') + BEGFOR - 1
         PEQUAL = INDEX (MODULE(BEGFOR:LMODUL), '=') + BEGFOR - 1
         IF (PCALL.LE.(BEGFOR-1)) PCALL = LMODUL
         IF (PEQUAL.LE.(BEGFOR-1)) PEQUAL = LMODUL
         PCALL = MIN (PCALL, PEQUAL)
         BEGFOR = LAST (MODULE(PCALL-72:PCALL), '@') + PCALL - 72
         END IF
 999  RETURN
      END
      SUBROUTINE FNDVAR (ISCHAR, PAGE, MODULE, LMODUL, BEGFOR, NUMVAR,
     *   VARDCL, NCHK, CHECK, VARARG, DEFDIM, NVARG, NVUSE, ARGVAR,
     *   VARUSE, VARDIM)
C-----------------------------------------------------------------------
C   Subroutine to find calls to specified routines and to return a list
C   of specified arguments.
C   Also logs conflicting useage.
C    Inputs:
C     ISCHAR   L  IF true then looking for CHARACTERS
C     PAGE     I  Number of pages in MODULE
C     MODULE   C  First page of the Module buffer.
C     LMODUL   I  Number of characters in MODULE.
C     BEGFOR   I  Start of executable code on first page.
C     NUMVAR   I  Number of elements in VARDCL
C     VARDCL   C  Specified routine names
C     NCHK     I  Number of elements in CHECK
C     CHECK    C  List of arguments declared another type.
C     VARARG() I  List of specified argument numbers
C     DEFDIM() I  Default number of characters.
C   Outputs:
C     NVARG    I  Number of arguments found
C     NVUSE    I  Number of calling routines
C     ARGVAR   C  List of Specified arguments
C     VARUSE   C  List of Specified routines called
C     VARDIM(*)C  List of dimensions to use (character only).
C                 'none' => use local declaration
C-----------------------------------------------------------------------
      INTEGER   PAGE
      LOGICAL   ISCHAR
      CHARACTER MODULE(PAGE)*(*), VARDCL(*)*6, ARGVAR(*)*6,
     *   CHECK(*)*6, VARUSE(*)*6, VARDIM(*)*20
      INTEGER   LMODUL(PAGE), BEGFOR, NUMVAR, VARARG(5,*), NCHK,
     *   NVARG, NVUSE,  DEFDIM(5,*)
      INTEGER   IPAGE, ISUBR, LOOP, FIRST, POINT, ITRIM, SLEN, NPARM,
     *   PLEN(100), IARG, JARG, OFF, LCALLS, START, LSUB, TPNT, ND
      LOGICAL   ISSUB
      CHARACTER PARM(100)*50, CALLS*15, LABEL*30, SUB*10, NEXTC
C-----------------------------------------------------------------------
      IF (NUMVAR.LE.0) GO TO 999
      START = BEGFOR
      NVUSE = 0
C                                       Loop over pages
      DO 500 IPAGE = 1,PAGE
C                                       Loop over Specified routines
         DO 400 ISUBR = 1,NUMVAR
            FIRST = START
            SLEN = ITRIM (VARDCL(ISUBR))
C                                       READ and WRITE no 'CALL';
            IF ((VARDCL(ISUBR)(1:5).EQ.'READ ') .OR.
     *          (VARDCL(ISUBR)(1:6).EQ.'WRITE ')) THEN
               LCALLS = 3 + SLEN
               CALLS(1:LCALLS) = ' ' // VARDCL(ISUBR)(1:SLEN) // ' ('
            ELSE
               LCALLS = 6 + SLEN
               CALLS(1:LCALLS) = ' CALL ' // VARDCL(ISUBR)
               END IF
C                                       See if this is the subroutine
               POINT = 0
               IF (IPAGE.EQ.1) THEN
                  LSUB = SLEN + 4
                  SUB = 'INE ' // VARDCL(ISUBR)
                  POINT = INDEX (MODULE(1)(1:72), SUB(1:LSUB))
C                                       Make sure of name
                  IF (POINT.GT.0) THEN
                     NEXTC = MODULE(1)(POINT+LSUB:POINT+LSUB)
                     IF ((NEXTC.NE.' ') .AND. (NEXTC.NE.'(')) POINT = 0
                     END IF
                  ISSUB = POINT .GT. 0
                  IF (ISSUB) THEN
                     FIRST = 1
                     GO TO 60
                     END IF
                  END IF
C                                       Find call
 50            POINT = INDEX (MODULE(IPAGE)(FIRST:LMODUL(IPAGE)),
     *            CALLS(1:LCALLS))
C                                       Found?
C                                       Verify name
               IF (POINT.GT.0) THEN
                  TPNT = FIRST + POINT + LCALLS - 1
                  NEXTC = MODULE(1)(TPNT:TPNT)
                  IF ((NEXTC.NE.' ') .AND. (NEXTC.NE.'(')) POINT = 0
                  END IF
 60            IF (POINT.GT.0) THEN
                  OFF = POINT
C                                       Save routine name
                  DO 100 LOOP = 1,NVUSE
                     IF (VARDCL(ISUBR).EQ.VARUSE(LOOP)) GO TO 110
 100                 CONTINUE
                  NVUSE = NVUSE + 1
                  VARUSE(NVUSE) = VARDCL(ISUBR)
C                                       Parse arguments
 110              POINT = POINT + FIRST - 1
                  CALL PARSEC (MODULE(IPAGE)(POINT:LMODUL(IPAGE)),
     *               NPARM, PARM, PLEN)
                  IF (NPARM.LE.0) GO TO 250
                  DO 200 IARG = 1,5
                     IF (VARARG(IARG,ISUBR).GT.0) THEN
C                                       Save argument
                        JARG = VARARG(IARG,ISUBR)
C                                       Only valid arguments
                        IF ((JARG.LE.NPARM) .AND. (PARM(JARG)(1:6).NE.
     *                   '      ')) THEN
C                                       Check for conflicts
                        DO 120 LOOP = 1,NCHK
                           IF (PARM(JARG).EQ.CHECK(LOOP)) THEN
                              LABEL = 'Type conflict in use of ' //
     *                           CHECK(LOOP)
                              CALL LOGER (MODULE(IPAGE), LMODUL(IPAGE),
     *                           POINT, LABEL)
                              GO TO 130
                              END IF
 120                       CONTINUE
 130                       DO 150 LOOP = 1,NVARG
                              IF (PARM(JARG)(1:6).EQ.ARGVAR(LOOP))
     *                           GO TO 200
 150                          CONTINUE
                           END IF
                        NVARG = NVARG + 1
                        ARGVAR(NVARG) = PARM(JARG)(1:6)
C                                       Character dimensions
                        IF (ISCHAR) THEN
                           ND = DEFDIM(IARG,ISUBR)
                           VARDIM(NVARG) = '*(*)'
                           IF (ND.EQ.0) VARDIM(NVARG) = 'none'
                           IF ((ND.GE.1) .AND. (ND.LE.9))
     *                        WRITE (VARDIM(NVARG),1150,ERR=200) ND
                           IF ((ND.GE.10) .AND. (ND.LE.99))
     *                        WRITE (VARDIM(NVARG),1151,ERR=200) ND
                           IF ((ND.GE.100) .AND. (ND.LE.999))
     *                        WRITE (VARDIM(NVARG),1152,ERR=200) ND
                           IF ((ND.GE.1000) .AND. (ND.LE.9999))
     *                        WRITE (VARDIM(NVARG),1153,ERR=200) ND
                           IF ((ND.GE.10000) .AND. (ND.LE.99999))
     *                        WRITE (VARDIM(NVARG),1154,ERR=200) ND
                           END IF
                        END IF
 200                 CONTINUE
C                                       See if more
 250              FIRST = FIRST + OFF + SLEN
                  IF (.NOT.ISSUB) GO TO 50
               END IF
 400        CONTINUE
         START = 1
 500     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('*',I1)
 1151 FORMAT ('*',I2)
 1152 FORMAT ('*',I3)
 1153 FORMAT ('*',I4)
 1154 FORMAT ('*',I5)
      END
      SUBROUTINE HOLDL (MODULE, LMODUL, DCLPOS, NUMDCL, NHARG, ARGHOL,
     *   NHKEEP, HKEEP, HKDIM, PRECUR, DCL, LDCL)
C-----------------------------------------------------------------------
C   Subroutine to redeclare locally declared variables as HOLLERITH.
C   Attempts to change comment in precursor comment.
C   Logs Hollerith declarations.
C    Inputs:
C     MODULE   C  First page of the Module buffer.
C     LMODUL   I  Number of characters in MODULE.
C     DCLPOS(3,*)  I  Declaration position pointers rel MODULE.
C                     First character, last character, type:
C                     1 = INTEGER, 2 = REAL, 3 = HOLLERITH,
C                     4 = CHARACTER, 5 = LOGICAL, 6 = DOUBLE PRECISION,
C                     7 = COMPLEX
C     NUMDCL   I  Number of values in DCLPOS
C     NHARG    I  Number of arguments found
C     ARGHOL   C  List of Hollerith arguments, blank filled.
C     NHKEEP   I  Number of values in HKEEP
C     HKEEP    C  Variables to declare if used even if not already
C                 locally declared.
C     HKDIM    C  Dimension string for KEEP arguments.
C     PRECUR   C  Precursor comments
C   Outputs:
C     DCL      C  Declaration string
C     LDCL     I  Length of DCL
C-----------------------------------------------------------------------
      CHARACTER MODULE*(*), ARGHOL(*)*6, DCL*(*), HKEEP(*)*6,
     *   HKDIM(*)*20, PRECUR*(*)
      INTEGER   LMODUL, NHARG, LDCL, DCLPOS(3,*), NUMDCL, NHKEEP
      INTEGER   LOOP, LSTR, IARG, TYPE, POINT, ITRIM, ILEN, LDSTR, TLEN,
     *   START, END, BPNT, IPNT, JPNT, KPNT, ICHA, ICHZ, HERE, IBC, IEC
      CHARACTER STRING*4096, DCLSTR*50, TSTRNG*2048, BC*1, EC*1, SSTR*7
C-----------------------------------------------------------------------
      IF ((NHARG.LE.0) .OR.(NUMDCL.LE.0)) GO TO 999
      ICHA = ICHAR ('A')
      ICHZ = ICHAR ('Z')
C                                       Loop over list building
C                                       declaration string
      LSTR = 0
      DO 500 IARG = 1,NHARG
         ILEN = ITRIM (ARGHOL(IARG))
         IF (ILEN.LE.0) GO TO 500
         HERE = 1
 40      POINT = INDEX (MODULE(HERE:LMODUL), ARGHOL(IARG)(1:ILEN))
         IF (POINT.GT.0) THEN
            POINT = POINT + HERE - 1
            HERE = POINT + ILEN
C                                       Make sure really have variable
            BC = MODULE(POINT-1:POINT-1)
            EC = MODULE(POINT+ILEN:POINT+ILEN)
            IBC = ICHAR (BC)
            IEC = ICHAR (EC)
            IF (((IBC.GE.ICHA).AND.(IBC.LE.ICHZ)) .OR.
     *          ((IEC.GE.ICHA).AND.(IEC.LE.ICHZ))) GO TO 40
C                                       Find type
            TYPE = 3
            DO 100 LOOP = 1,NUMDCL
               IF ((POINT.GE.DCLPOS(1,LOOP)) .AND.
     *             (POINT.LE.DCLPOS(2,LOOP))) THEN
                  TYPE = DCLPOS(3,LOOP)
                  BPNT = DCLPOS(1,LOOP) + 1
                  GO TO 110
                  END IF
 100           CONTINUE
C                                       Not found, try again
            GO TO 40
C                                       Ignore HOLLERITH varaiable
 110        IF ((TYPE.EQ.3) .OR. (TYPE.LE.0) .OR. (TYPE.GT.7)) GO TO 490
C                                       Get declaration/ blank old
            CALL PARSEA (MODULE(POINT:POINT+200), TYPE,
     *         MODULE(BPNT:POINT-1), DCLSTR, LDSTR)
C                                       Form declaration
            TLEN = ILEN + LDSTR
            TSTRNG(1:TLEN) = ARGHOL(IARG)(1:ILEN) // DCLSTR(1:LDSTR)
         ELSE
C                                       See if keep anyway
            DO 200 LOOP = 1,NHKEEP
               TYPE = LOOP
               IF (ARGHOL(IARG).EQ.HKEEP(LOOP)) GO TO 210
 200           CONTINUE
C                                       Bag it
            GO TO 490
C                                       Get declaration
 210        LDSTR = ITRIM (HKDIM(TYPE))
            TLEN = LDSTR + ILEN
            TSTRNG(1:TLEN) = ARGHOL(IARG)(1:ILEN) //
     *         HKDIM(TYPE)(1:LDSTR)
            END IF
C                                       Form declaration string
C                                       First argument
         IF (LSTR.LE.0) THEN
C                                       Need to indent?
            IF (LDCL.LE.0) THEN
               LSTR = 16 + TLEN
               STRING(1:LSTR) = '      HOLLERITH ' // TSTRNG(1:TLEN)
               START = 7
            ELSE
               LSTR = 10 + TLEN
               STRING(1:LSTR) = 'HOLLERITH ' // TSTRNG(1:TLEN)
               START = 1
               END IF
C                                       Add one
         ELSE
            STRING(LSTR+1:LSTR+TLEN+2) = ', ' // TSTRNG(1:TLEN)
            LSTR = LSTR + TLEN + 2
            END IF
C                                       Change precursor comments
 490     SSTR = ' ' // ARGHOL(IARG)
         IPNT = INDEX (PRECUR, SSTR(1:ILEN+1))
         IF (IPNT.GT.0) THEN
            JPNT = INDEX (PRECUR(IPNT:IPNT+75), '@') + IPNT - 1
            KPNT = INDEX (PRECUR(IPNT:JPNT), ' R')
            IF (KPNT.LE.0) KPNT = INDEX (PRECUR(IPNT:JPNT), ' I')
            IF (KPNT.GT.0) THEN
               KPNT = IPNT + KPNT - 1
               EC = PRECUR(KPNT+2:KPNT+2)
               IF ((EC.EQ.' ') .OR. (EC.EQ.'('))
     *            PRECUR(KPNT+1:KPNT+1) = 'H'
               END IF
            END IF
 500     CONTINUE
C                                       Done any?
      IF (LSTR.LE.0) GO TO 999
C                                       Add to DCL
      IF (LDCL.LE.0) THEN
         DCL = STRING(1:LSTR)
         LDCL = LSTR
      ELSE
         DCL(LDCL+1:LDCL+LSTR+1) = '@' // STRING(1:LSTR)
         LDCL = LDCL + LSTR + 1
         END IF
C                                       Log Hollerith Declarations
C                                       Format line
      TLEN = 1
      CALL LINOUT (STRING, LSTR, TSTRNG, TLEN)
C                                       Print in LOG file
      END = INDEX (TSTRNG(START:START+75), '@') + START - 2
      IF (END.GE.START) WRITE (12,1600) TSTRNG(START:END)
      START = END + 2
 600     IF (START.GE.TLEN) GO TO 999
         END = INDEX (TSTRNG(START:START+75), '@') + START - 2
         IF (END.GE.START) WRITE (12,1601) TSTRNG(START:END)
         START = END + 2
         GO TO 600
C
 999  RETURN
C-----------------------------------------------------------------------
 1600 FORMAT (/' New declaration          : ',A)
 1601 FORMAT ( '                          : ',A)
      END
      SUBROUTINE DCLWRN (PAGE, START, MODULE, LMODUL, NARG, NUSE,
     *   ARG, USE, LABEL)
C-----------------------------------------------------------------------
C   Subroutine to log warnings about uses of variables in the list ARG
C   which don't appear on the same line as a value in USE.
C    Inputs:
C     PAGE     I  Number of pages in MODULE
C     START    I  First character to consider on page 1
C     MODULE   C  First page of the Module buffer.
C     LMODUL   I  Number of characters in MODULE.
C     NARG     I  Number of values in ARG
C     NUSE     I  Number of values in USE
C     ARG      C  List of arguments to check, blank filled.
C     USE      C  Subroutine names which should use ARG, blank filled.
C     LABEL    C  Label for log
C-----------------------------------------------------------------------
      INTEGER   PAGE, START, NARG, NUSE
      CHARACTER MODULE(PAGE)*(*), ARG(*)*6, USE(*)*6, LABEL*10
      INTEGER   LMODUL(PAGE)
      CHARACTER ARGSTR*10, OLABEL*25
      INTEGER   FIRST, LOOP, IPAGE, IARG, BACK, POINT, IPNT, ITRIM,
     *   LARG, LINEND(2,20), FRONT, END, LAST, HERE, BCKCHK, IUSE,
     *   LUSE(500), EEND, N72, ICHA, ICHZ, ICHBEG, ICHEND
      DATA N72 /72/
C-----------------------------------------------------------------------
      IF (NARG.LE.0) GO TO 999
C                                       Get values for "A", "Z"
      ICHA = ICHAR ('A')
      ICHZ = ICHAR ('Z')
      FIRST = START
C                                       Fill length array
      DO 20 LOOP = 1,NUSE
         LUSE(LOOP) = ITRIM (USE(LOOP))
 20      CONTINUE
C                                       Loop over pages
      DO 500 IPAGE = 1,PAGE
C                                       Loop over arguments
         DO 400 IARG = 1,NARG
            POINT = FIRST
C                                       Find occurance
            LARG = ITRIM (ARG(IARG))
            IF (LARG.LE.0) GO TO 400
 100           IPNT = INDEX (MODULE(IPAGE)(POINT:LMODUL(IPAGE)),
     *            ARG(IARG)(1:LARG))
               IF (IPNT.LE.0) GO TO 400
C                                       Save argument
               ARGSTR = MODULE(IPAGE)(POINT+IPNT-2:POINT+IPNT+LARG)
               BACK = 0
C                                       Find end of line
               HERE = POINT + IPNT - 1
               END = INDEX (MODULE(IPAGE)(HERE:HERE+75), '@') + HERE - 1
               EEND = END
C                                       Reset POINT for next search
               POINT = END + 1
C                                       Make sure have the variable
               ICHBEG = ICHAR (ARGSTR(1:1))
               ICHEND = ICHAR (ARGSTR(LARG+2:LARG+2))
C                                       Are prev or following letters?
               IF (((ICHBEG.GE.ICHA) .AND. (ICHBEG.LE.ICHZ)) .OR.
     *             ((ICHEND.GE.ICHA) .AND. (ICHEND.LE.ICHZ))) GO TO 100
C                                       READ or WRITE
               IF (ARGSTR(1:7).EQ.' READ (') GO TO 100
               IF (ARGSTR(1:8).EQ.' WRITE (') GO TO 100
C                                       Comment or FORMAT?
               BCKCHK = MIN (N72, HERE) - 1
               FRONT = LAST (MODULE(IPAGE)(HERE-BCKCHK:HERE), '@')
     *            + HERE - BCKCHK - 1
               IF (MODULE(IPAGE)(FRONT+1:FRONT+1).EQ.'C') GO TO 100
C                                       Backup to first line of
C                                       continuation series.
C                                       Find end
 150              BCKCHK = MIN (N72, HERE) - 1
                  FRONT = LAST (MODULE(IPAGE)(HERE-BCKCHK:HERE), '@')
     *               + HERE - BCKCHK - 1
C                                       Save ends
                  BACK = BACK + 1
                  LINEND(1,BACK) = FRONT
                  LINEND(2,BACK) = END
                  HERE = FRONT - 2
                  END = FRONT
C                                       Continuation line? loop
                  IF ((MODULE(IPAGE)(FRONT+6:FRONT+6).NE.' ') .AND.
     *               (HERE.GT.0)) GO TO 150
C                                       FORMAT?
               IF (MODULE(IPAGE)(FRONT+7:FRONT+12).EQ.'FORMAT')
     *            GO TO 300
C                                       Look for approved name
               DO 200 IUSE = 1,NUSE
                  IPNT = INDEX (MODULE(IPAGE)(FRONT+1:EEND-1),
     *               USE(IUSE)(1:LUSE(IUSE)))
                  IF (IPNT.GT.0) GO TO 300
 200              CONTINUE
C                                       None, log it.
               HERE = FRONT + 6
               OLABEL = 'Use of ' // LABEL // ARG(IARG)
               CALL LOGER (MODULE(IPAGE), LMODUL(IPAGE), HERE, OLABEL)
 300           GO TO 100
 400        CONTINUE
         FIRST = 1
 500     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CHRDL (MODULE, LMODUL, DCLPOS, NUMDCL, NUMCHR, CHRARG,
     *   NCARG, ARGCHR, CHRDIM, NCKEEP, CKEEP, CKDIM, PRECUR, DCL,
     *   LDCL)
C-----------------------------------------------------------------------
C   Subroutine to redeclare locally declared variables as CHARACTER.
C   Logs Character declarations.
C    Inputs:
C     MODULE   C  First page of the Module buffer.
C     LMODUL   I  Number of characters in MODULE.
C     DCLPOS(3,*)  I  Declaration position pointers rel MODULE.
C                     First character, last character, type:
C                     1 = INTEGER, 2 = REAL, 3 = HOLLERITH,
C                     4 = CHARACTER, 5 = LOGICAL, 6 = DOUBLE PRECISION,
C                     7 = COMPLEX
C     NUMDCL   I  Number of values in DCLPOS
C     NUMCHR   I  Number of elements in CHRARG
C     CHRARG   I  (arg,subroutine) argument number
C     NCARG    I  Number of arguments found
C     ARGCHR   C  List of CHARACTER arguments, blank filled.
C     CHRDIM   C  Dimensions for ARGCHR
C     NCKEEP   I  Number of values in CKEEP
C     CKEEP    C  Variables to declare if used even if not already
C                 locally declared.
C     CKDIM    C  Dimensions to use for CKEEP if not otherwise defined.
C     PRECUR   C  Precursor comments
C   Outputs:
C     DCL      C  Declaration string
C     LDCL     I  Length of DCL
C-----------------------------------------------------------------------
      CHARACTER MODULE*(*), ARGCHR(*)*6, CHRDIM(*)*20, CKEEP(*)*6,
     *   CKDIM(*)*20, DCL*(*), PRECUR*(*)
      INTEGER   LMODUL, DCLPOS(3,*), NUMDCL, NUMCHR, CHRARG(5,200,2),
     *   NCARG, NCKEEP, LDCL
      INTEGER   LOOP, LSTR, IARG, TYPE, POINT, ITRIM, ILEN, LDSTR, TLEN,
     *   START, END, NDIG, JPNT, LENDCL, BPNT, IPNT, LPNT, LABLEN,
     *   ICHA, ICHZ, HERE, IBC, IEC, KPNT, JLEN, LDIM, LAST
      CHARACTER STRING*4096, DCLSTR*50, TSTRNG*2048, TEMP*6, LABEL*256,
     *   BC*1, EC*1, SSTR*7, DSTR*20
C-----------------------------------------------------------------------
      IF (NCARG.LE.0) GO TO 999
      ICHA = ICHAR ('A')
      ICHZ = ICHAR ('Z')
      TLEN = 0
C                                       Loop over list building
C                                       declaration string
      LSTR = 0
      DO 500 IARG = 1,NCARG
         LDSTR = 2
         DCLSTR = '*?'
         ILEN = ITRIM (ARGCHR(IARG))
         IF (ILEN.LE.0) GO TO 500
         HERE = 1
 40      POINT = INDEX (MODULE(HERE:LMODUL), ARGCHR(IARG)(1:ILEN))
         IF (POINT.GT.0) THEN
            POINT = POINT + HERE - 1
            HERE = POINT + ILEN
C                                       Make sure really have variable
            BC = MODULE(POINT-1:POINT-1)
            EC = MODULE(POINT+ILEN:POINT+ILEN)
            IBC = ICHAR (BC)
            IEC = ICHAR (EC)
            IF (((IBC.GE.ICHA).AND.(IBC.LE.ICHZ)) .OR.
     *          ((IEC.GE.ICHA).AND.(IEC.LE.ICHZ))) GO TO 40
C                                       Find type
            TYPE = 4
            DO 100 LOOP = 1,NUMDCL
               IF ((POINT.GE.DCLPOS(1,LOOP)) .AND.
     *             (POINT.LE.DCLPOS(2,LOOP))) THEN
                  TYPE = DCLPOS(3,LOOP)
                  BPNT = DCLPOS(1,LOOP) + 1
                  GO TO 110
                  END IF
 100           CONTINUE
C                                       Not found, try again
            GO TO 40
C                                       Ignore CHARACTER varaiable
 110        IF ((TYPE.EQ.4) .OR. (TYPE.LE.0) .OR. (TYPE.GT.7)) GO TO 490
C                                       Blank old declaration
            CALL PARSEA (MODULE(POINT:POINT+200), TYPE,
     *         MODULE(BPNT:POINT-1), DCLSTR, LDSTR)
C                                       Number of characters for type
            DSTR = '*4'
            IF (TYPE.EQ.1) DSTR = '*2'
            IF (TYPE.EQ.6) DSTR = '*8'
C                                       From subroutine declaration?
            LDIM = ITRIM (CHRDIM(IARG))
            IF (CHRDIM(IARG)(1:4) .EQ. 'none') LDIM = 0
            IF (LDIM.GT.0) THEN
               DSTR(1:LDIM) = CHRDIM(IARG)(1:LDIM)
            ELSE
               LDIM = 2
               END IF
C                                       Add *4 etc. (not arrays)
            IF (LDSTR.LE.0) THEN
               DCLSTR(LDSTR+1:LDSTR+LDIM) = DSTR(1:LDIM)
               LDSTR = LDSTR + LDIM
C                                       (arrays add ?)
            ELSE
               DCLSTR(LDSTR+1:LDSTR+LDIM+1) = DSTR(1:LDIM) // '?'
               LDSTR = LDSTR + LDIM + 1
               END IF
         ELSE
C                                       See if keep anyway
            DO 200 LOOP = 1,NCKEEP
               TYPE = LOOP
               IF (ARGCHR(IARG).EQ.CKEEP(LOOP)) THEN
                  LDSTR = ITRIM (CKDIM(LOOP))
                  DCLSTR(1:LDSTR) = CKDIM(LOOP)(1:LDSTR)
                  GO TO 210
                  END IF
 200           CONTINUE
C                                       Bag it
            GO TO 500
            END IF
C                                       Form declaration
 210     TSTRNG(1:ILEN) = ARGCHR(IARG)(1:ILEN)
         TLEN = ILEN + LDSTR
         TSTRNG(ILEN+1:TLEN) = DCLSTR(1:LDSTR)
C                                       Form declaration string
C                                       First argument?
         IF (LSTR.LE.0) THEN
C                                       Need to indent?
            IF (LDCL.LE.0) THEN
               LSTR = 16 + TLEN
               STRING(1:LSTR) = '      CHARACTER ' // TSTRNG(1:TLEN)
               START = 7
            ELSE
               LSTR = 10 + TLEN
               STRING(1:LSTR) = 'CHARACTER ' // TSTRNG(1:TLEN)
               START = 1
               END IF
C                                       Add one
         ELSE
            STRING(LSTR+1:LSTR+TLEN+2) = ', ' // TSTRNG(1:TLEN)
            LSTR = LSTR + TLEN + 2
            END IF
C                                       Change precursor comments
 490     SSTR = ' ' // ARGCHR(IARG)
         IPNT = INDEX (PRECUR, SSTR(1:ILEN+1))
         IF (IPNT.GT.0) THEN
C                                       Get no char.
            LPNT = LAST (TSTRNG(1:TLEN), '*')
            IF (LPNT.LE.0) LPNT = TLEN
            LABLEN = TLEN - LPNT + 1
            LABEL(1:LABLEN) = TSTRNG(LPNT:TLEN)
            JPNT = INDEX (PRECUR(IPNT:IPNT+75), '@') + IPNT - 1
            KPNT = INDEX (PRECUR(IPNT:JPNT), ' R')
            IF (KPNT.LE.0) KPNT = INDEX (PRECUR(IPNT:JPNT), ' I')
            IF (KPNT.LE.0) KPNT = INDEX (PRECUR(IPNT:JPNT), ' C')
            IF (KPNT.GT.0) THEN
               KPNT = IPNT + KPNT - 1
               EC = PRECUR(KPNT+2:KPNT+2)
               IF ((EC.EQ.' ') .OR. (EC.EQ.'('))
     *            PRECUR(KPNT+1:KPNT+LABLEN+1) = 'C' // LABEL(1:LABLEN)
               END IF
            END IF
 500     CONTINUE
C                                       Done any?
      IF (LSTR.LE.0) GO TO 999
C                                       Add to DCL
      IF (LDCL.LE.0) THEN
         DCL = STRING(1:LSTR)
         LDCL = LSTR
      ELSE
         DCL(LDCL+1:LDCL+LSTR+1) = '@' // STRING(1:LSTR)
         LDCL = LDCL + LSTR + 1
         END IF
C                                       Log Character Declarations
C                                       Format line
      TLEN = 1
      CALL LINOUT (STRING, LSTR, TSTRNG, TLEN)
C                                       Print in LOG file
      END = INDEX (TSTRNG(START:START+75), '@') + START - 2
      IF (END.GE.START) WRITE (12,1600) TSTRNG(START:END)
      START = END + 2
 600     IF (START.GE.TLEN) GO TO 999
         END = INDEX (TSTRNG(START:START+75), '@') + START - 2
         IF (END.GE.START) WRITE (12,1601) TSTRNG(START:END)
         START = END + 2
         GO TO 600
C
 999  RETURN
C-----------------------------------------------------------------------
 1110 FORMAT (I6)
 1600 FORMAT (/' New declaration          : ',A)
 1601 FORMAT ( '                          : ',A)
      END
      SUBROUTINE PARSEA (STRING, TYPE, REST, DCLSTR, LDSTR)
C-----------------------------------------------------------------------
C   Parse a declaration from a character array, the old declaration is
C   blanked.
C   '@' is assumed to the the end of line character.
C    Input
C     STRING   C  String to parse, starting with varaible name.
C     TYPE     I  Data type, INTEGER, REAL, HOLLERITH, CHARACTER,
C                 LOGICAL, DOUBLE PRECISION, COMPLEX
C     REST     C  Beginning of line before STRING.
C    Output:
C     DCLSTR   C  Output declaration string
C     LDSTR    I  Number of characters in DCLSTR
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), REST*(*), DCLSTR*(*)
      INTEGER   TYPE, LDSTR
      CHARACTER WORK*1024, TSTR*50, BLANK*50, CHR*1
      INTEGER   END, IPNT, PLEVEL, LOOP, LENG, JPNT, LOOP2, ITRIM,
     *   POINT, KPNT, IVALUE, NDIG, OVALUE, BPOINT, EPNT, START, LAST,
     *   IEND
      LOGICAL   LASTD
      DATA BLANK /'                                                  '/
C-----------------------------------------------------------------------
      POINT = 1
      LDSTR = 0
C                                       Extract rest of current line
      IPNT = INDEX (STRING(POINT:), '@')
      WORK = STRING(POINT:POINT+IPNT-1)
      END = IPNT
      IEND = IPNT
      POINT = POINT + IPNT
C                                       Start at first '(' before ','
      IPNT = INDEX (WORK(1:END), '(')
      KPNT = INDEX (WORK(1:END), ',')
      IF (KPNT.LE.0) KPNT = END + 1
      IF ((IPNT.LE.0) .OR. (KPNT.LT.IPNT)) THEN
C                                       See if last argument
         LASTD = INDEX (WORK(1:END), ',') .LE. 0
         GO TO 900
         END IF
C                                       Blank name
      STRING(1:IPNT) = BLANK(1:IPNT)
      BPOINT = IPNT
      PLEVEL = 1
      LDSTR = 1
      DCLSTR(LDSTR:LDSTR) = WORK(IPNT:IPNT)
C                                       Find next level 0 ','
 100  DO 200 LOOP = IPNT+1,END
         JPNT = LOOP
         IF (WORK(LOOP:LOOP).EQ.'(') PLEVEL = PLEVEL + 1
         IF (WORK(LOOP:LOOP).EQ.')') PLEVEL = PLEVEL - 1
C                                       Done?
         IF ((WORK(LOOP:LOOP).EQ.',') .AND. (PLEVEL.EQ.0)) GO TO 500
         IF ((WORK(LOOP:LOOP).EQ.'@') .AND. (PLEVEL.EQ.0)) GO TO 500
         IF (WORK(LOOP:LOOP) .NE. ' ') THEN
            LDSTR = LDSTR + 1
            DCLSTR(LDSTR:LDSTR) = WORK(LOOP:LOOP)
            END IF
 200     CONTINUE
      JPNT = END
C                                       Not done;
C                                       fetch another line.
      IPNT = INDEX (STRING(POINT+1:POINT+75), '@') + 1
      WORK = STRING(POINT+1:POINT+IPNT-2)
C                                       If not a continuation then done
      IF ((WORK(6:6).EQ.' ') .OR. (WORK(1:1).EQ.'C')) GO TO 500
      END = IPNT
      POINT = POINT + IPNT
C                                       Blank declaration
      EPNT = BPOINT + JPNT - IPNT
      IF (STRING(EPNT:EPNT).EQ.'@') EPNT = EPNT - 1
      STRING(BPOINT:EPNT) = BLANK
      BPOINT = POINT
      IPNT = 6
C                                       Continue on next line
      GO TO 100
C                                       Blank declaration
 500  EPNT = BPOINT + JPNT - IPNT
      IF (STRING(EPNT:EPNT).EQ.'@') EPNT = EPNT - 1
      STRING(BPOINT:EPNT) = BLANK
C                                       If last declaration on line
C                                       remove last comma or whole line.
      LASTD = WORK(JPNT:JPNT) .EQ. '@'
      IF (LASTD) THEN
            LENG = ITRIM (REST)
            IPNT = LAST (REST(1:LENG), ',')
            IF (IPNT.LE.0) THEN
               IPNT =  LAST (REST(1:LENG), '@')
               IF (IPNT.EQ.1) IPNT = 2
               END IF
            REST(IPNT:LENG) = BLANK
         END IF
      IF (TYPE.NE.1) GO TO 950
C                                       Must change INTEGER declarations
C                                       Get first value
C                                       Don't re dimension for now...
C      IPNT = INDEX (DCLSTR, ',')
C      IF (IPNT.LE.0) IPNT = INDEX (DCLSTR, ')')
CC                                       Can't cope?
C      IF (IPNT.LE.0) GO TO 950
C      TSTR(1:8) = '  '
C      NDIG = 0
C      DO 550 LOOP = 1,IPNT-1
C         JPNT = IPNT - LOOP
C         KPNT = 7 - LOOP
C         CHR = DCLSTR(JPNT:JPNT)
C         IF ((CHR.EQ.'1') .OR. (CHR.EQ.'2') .OR. (CHR.EQ.'3') .OR.
C     *   (CHR.EQ.'4') .OR. (CHR.EQ.'5') .OR. (CHR.EQ.'6') .OR.
C     *   (CHR.EQ.'7') .OR. (CHR.EQ.'8') .OR. (CHR.EQ.'9') .OR.
C     *   (CHR.EQ.'0')) THEN
C            TSTR(KPNT:KPNT) = CHR
C            NDIG = NDIG + 1
C            END IF
C 550     CONTINUE
CC                                       Bail out if NDIG .le. 0
C      IF (NDIG.LE.0) GO TO 950
CC                                       Compute number needed changing
CC                                       from 2 to word to 4.
C      READ (TSTR(1:6),1550) OVALUE
C      IVALUE = OVALUE * 2
C      IVALUE = (IVALUE / 4.0) + 0.99
C      WRITE (TSTR(1:6),1550) IVALUE
CC                                       Change declaration  (1) to (*)
C      IF (OVALUE.EQ.1) TSTR(6:6) = '*'
CC                                       Insert new value
C      DCLSTR(2:NDIG+1) = TSTR(7-NDIG:6)
      GO TO 950
C                                       Blank simple name, look for ','
C                                       or '@'
 900  DO 920 LOOP = 1,END
         IF (STRING(LOOP:LOOP).EQ.',') THEN
            STRING(LOOP:LOOP) = ' '
            GO TO 930
            END IF
         IF (STRING(LOOP:LOOP).EQ.'@') GO TO 930
         STRING(LOOP:LOOP) = ' '
 920     CONTINUE
C                                       If last declaration on line
C                                       remove last comma or whole line.
 930  IF (LASTD) THEN
            LENG = ITRIM (REST)
            IPNT = LAST (REST(1:LENG), ',')
            IF (IPNT.LE.0) THEN
               IPNT =  LAST (REST(1:LENG), '@')
               IF (IPNT.EQ.1) IPNT = 2
               END IF
            REST(IPNT:LENG) = BLANK
         END IF
C                                       Beautify (remove excess blanks)
 950  IEND = IEND - 1
      DO 980 LOOP = 2,IEND
         IF (STRING(LOOP-1:LOOP).EQ.'  ') THEN
C                                       Shift
 960        DO 970 LOOP2 = LOOP,IEND-1
               STRING(LOOP2:LOOP2) = STRING(LOOP2+1:LOOP2+1)
 970           CONTINUE
            STRING(IEND:IEND) = ' '
C                                       More blanks?
            IPNT = ITRIM (STRING(LOOP:IEND))
            IF (IPNT.LE.0) GO TO 985
            IF (STRING(LOOP-1:LOOP).EQ.'  ') GO TO 960
            END IF
 980     CONTINUE
C                                       Remove initial blank
 985  IF (STRING(1:1).EQ.' ') THEN
         DO 990 LOOP2 = 1,IEND-1
            STRING(LOOP2:LOOP2) = STRING(LOOP2+1:LOOP2+1)
 990        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1550 FORMAT (I6)
      END
      SUBROUTINE DATCHR (MODULE, LMODUL, BEGFOR, BEGDCL, DCLPOS, NUMDCL,
     *   NVARG, ARGVAR, CHRDIM)
C-----------------------------------------------------------------------
C   Subroutine to find variables DATAed as character so they can be
C   redeclared.
C    Inputs:
C     MODULE   C  First page of the Module buffer.
C     LMODUL   I  Number of characters in MODULE.
C     BEGFOR   I  Start of executable code on first page.
C     BEGDCL   I  Start of declarations
C     DCLPOS(3,*)  I  Declaration position pointers rel BEGDCL.
C                     First character, last character, type:
C                     1 = INTEGER, 2 = REAL, 3 = HOLLERITH,
C                     4 = CHARACTER, 5 = LOGICAL, 6 = DOUBLE PRECISION,
C                     7 = COMPLEX
C     NUMDCL   I  Number of values in DCLPOS
C   Outputs:
C     NVARG    I  Number of arguments found
C     ARGVAR   C  List of Specified arguments
C     CHRDIM   C  List of dimensions of ARGVAR
C-----------------------------------------------------------------------
      CHARACTER MODULE*(*), ARGVAR(*)*6, CHRDIM(*)*20
      INTEGER   LMODUL, BEGFOR, BEGDCL, NVARG, DCLPOS(3,*), NUMDCL
      INTEGER   LOOP, LOOP2, POINT, IPNT, JPNT, KPNT, START,
     *   DATTYP(100), NUMDAT
      CHARACTER EC*1, DATNAM(100)*6, DATDIM(100)*20
C-----------------------------------------------------------------------
      IF (BEGFOR.LE.DCLPOS(1,1)) GO TO 999
C                                       Look for DATA statment
      START = DCLPOS(2,NUMDCL)
      IPNT = INDEX (MODULE(START:BEGFOR), '@      DATA')
 100     IF (IPNT.GT.0) THEN
            POINT = START + IPNT + 10
            EC = MODULE(POINT:POINT)
C                                       Parse DATA statment
            IF (EC.EQ.' ') THEN
 110           CALL PARDAT (MODULE, POINT, BEGFOR, BEGDCL, NUMDCL,
     *            DCLPOS, NUMDAT, DATNAM, DATTYP, DATDIM)
C                                       Any character?
               DO 200 LOOP = 1,NUMDAT
                  IF (DATTYP(LOOP).EQ.4) THEN
C                                       Save
                     DO 150 LOOP2 = 1,NVARG
                        IF (DATNAM(LOOP).EQ.ARGVAR(LOOP2)) GO TO 200
 150                    CONTINUE
                     NVARG = NVARG + 1
                     ARGVAR(NVARG) = DATNAM(LOOP)
                     CHRDIM(NVARG) = DATDIM(LOOP)
                     END IF
 200              CONTINUE
C                                       More in DATA stmt?
               JPNT = INDEX (MODULE(POINT:LMODUL), ',')
               KPNT = INDEX (MODULE(POINT:LMODUL), '@')
               IF ((JPNT.LT.KPNT) .AND. (JPNT.GT.0)) THEN
                  POINT = POINT + JPNT
                  GO TO 110
                  END IF
               END IF
            START = POINT
            END IF
C                                       More DATA stmts?
         IPNT = 0
         IF (START.LT.BEGFOR)
     *      IPNT = INDEX (MODULE(START:BEGFOR), '@      DATA')
         IF (IPNT.GT.0) GO TO 100
C
 999  RETURN
      END
      SUBROUTINE PARDAT (MODULE, POINT, BEGFOR, BEGDCL, NUMDCL, DCLPOS,
     *   NUMDAT, DATNAM, DATTYP, DATDIM)
C-----------------------------------------------------------------------
C   Parse a DATA statment
C    Input
C     MODULE   C  String to parse.
C     DCLPOS(3,*)  I  Declaration position pointers rel BEGDCL.
C                     First character, last character, type:
C                     1 = INTEGER, 2 = REAL, 3 = HOLLERITH,
C                     4 = CHARACTER, 5 = LOGICAL, 6 = DOUBLE PRECISION,
C                     7 = COMPLEX
C     NUMDCL   I  Number of values in DCLPOS
C     BEGFOR   I  Start of executable code on first page.
C     BEGDCL   I  Start of declarations
C    Input/output:
C     POINT     I  Start character in MODULE for next
C     NUMDAT    I  Number of elements in DATNAM, DATNUM
C     DATNAM(*) C  variable names
C     DATTYP(*) I  Data type of DATA value in order:
C                  INTEGER, REAL, HOLLERITH, CHARACTER,
C                  LOGICAL, DOUBLE PRECISION, COMPLEX
C     DATDIM(*) C  Dimension for DATNAM
C-----------------------------------------------------------------------
      CHARACTER MODULE*(*), DATNAM(*)*6, DATDIM(*)*20
      INTEGER   POINT, BEGFOR, BEGDCL, NUMDCL, DCLPOS(3,*), NUMDAT,
     *   DATTYP(*)
      CHARACTER WORK*80, CHR*1, CHR2*1, BC*1, EC*1, DIMS(7)*50,
     *   VSTRNG*6, GOODCH*5
      INTEGER   END, IPNT, PLEVEL, LOOP, LENG, JPNT, LOOP2, NEXTNQ,
     *   HERE, KPNT, IVALUE, NDIG, OVALUE, BHERE, EPNT, START, LAST,
     *   IEND, ICHA, ICHZ, ICNEXT, LNAME, ICHBC, ICHEC, ISTART, NDIM,
     *   DLEN(7), VALUE, DIM, IP, JP, KP, LP, LIMIT, TYPE, NODAT, NUMDT,
     *   TEMP, TEMP2, NOVAR, NCOPY, DATNUM, NDATA
      LOGICAL   LASTD, GOTCHR, RECOVE
      EXTERNAL NEXTNQ
      DATA GOODCH /'(, *@'/
C-----------------------------------------------------------------------
      ICHA = ICHAR('A')
      ICHZ = ICHAR('Z')
      HERE = POINT
      NUMDT = 0
      NDATA = 0
      IPNT = NEXTNQ (MODULE(HERE:HERE+75), '@')
C                                       Trap end of line
      IF (IPNT.EQ.1) THEN
         HERE = HERE + 7
         IPNT = NEXTNQ (MODULE(HERE:HERE+74), '@')
         END IF
      IF (IPNT.LE.0) IPNT = INDEX (MODULE(HERE:HERE+75), '@')
      WORK = MODULE(HERE:HERE+IPNT-1)
      END = IPNT
      IEND = IPNT
      JPNT = 1
      NUMDAT = 1
      DATNAM(NUMDAT) = '      '
      LNAME = 0
      GOTCHR = .FALSE.
C                                       Find next variable
 50      CHR = WORK(JPNT:JPNT)
C                                       End of name?
         IF ((CHR.EQ.',') .OR. (CHR.EQ.'/')) GO TO 100
C                                       End of line
         IF (CHR.EQ.'@') GO TO 80
         ICNEXT = ICHAR (CHR)
         GOTCHR = GOTCHR .OR. ((ICNEXT.GE.ICHA).AND.(ICNEXT.LE.ICHZ))
         IF (GOTCHR .AND. (CHR.NE.' ') .AND. (LNAME.LE.5)) THEN
            LNAME = LNAME + 1
            DATNAM(NUMDAT)(LNAME:LNAME) = CHR
            END IF
         JPNT = JPNT + 1
C                                       Next character
         IF (JPNT.LE.END) GO TO 50
C                                       Another line
 80      HERE = HERE + END
         IPNT = NEXTNQ (MODULE(HERE+1:HERE+75), '@')
         IF (IPNT.LE.0) IPNT = INDEX (MODULE(HERE+1:HERE+75), '@')
         IPNT = IPNT + 1
         WORK = MODULE(HERE+7:HERE+IPNT-2)
         END = IPNT - 7
         JPNT = 1
         HERE = HERE + 7
         GO TO 50
C                                       Finished variable name, get
C                                       dimension
C                                       Got name?
 100  IF (LNAME.LE.0) GO TO 900
C                                       Find declaration of name
      ISTART = BEGDCL
 110  IPNT = INDEX (MODULE(ISTART:BEGFOR), DATNAM(NUMDAT)(1:LNAME))
      IF (IPNT.GT.0) THEN
C                                       In declaration stmt?
         KPNT = IPNT + ISTART - BEGDCL
         DO 120 LOOP = 1,NUMDCL
            TYPE = DCLPOS(3,LOOP)
            IF ((KPNT.GT.DCLPOS(1,LOOP)) .AND.
     *          (KPNT.LT.DCLPOS(2,LOOP))) GO TO 130
 120        CONTINUE
C                                       Nope, try again
         ISTART = ISTART + IPNT + LNAME
         GO TO 110
C                                       Verify name
 130        IPNT = IPNT + ISTART - 1
            ISTART = IPNT + LNAME
            EC = MODULE(IPNT-1:IPNT-1)
            BC = MODULE(IPNT+LNAME:IPNT+LNAME)
            ICHEC = ICHAR (EC)
            ICHBC = ICHAR (BC)
            IF (((ICHEC.GE.ICHA) .AND. (ICHEC.LE.ICHZ)) .OR.
     *          ((ICHBC.GE.ICHA) .AND. (ICHBC.LE.ICHZ))) GO TO 110
C                                       BC must valid
            IF (INDEX (GOODCH, BC) .LE.0) GO TO 110
            DATDIM(NUMDAT) = 'none'
C                                       Parse dimensions
            IF (BC.EQ.'*') THEN
C                                       Character
               NDIM = 1
               DIMS(1)='1'
               DLEN(1) = 1
            ELSE
C                                       Other types
               CALL PARSEC (MODULE(IPNT+LNAME:BEGFOR), NDIM, DIMS, DLEN)
               END IF
C                                       loop over dimensions
            DIM = 1
            DO 160 LOOP = 1,NDIM
               VSTRNG = '      '
C                                       Check if too long
               IF (DLEN(LOOP).GT.6) GO TO 900
               VSTRNG(6-DLEN(LOOP)+1:6) = DIMS(LOOP)(1:DLEN(LOOP))
C                                       Interprete
               READ (VSTRNG,1000,ERR=900) VALUE
               DIM = DIM * VALUE
 160           CONTINUE
C                                       Save type
            DATTYP(NUMDAT) = TYPE
            NUMDT = NUMDT + DIM
      ELSE
C                                       Not declared locally, bail out
         WRITE (6,1180) DATNAM(NUMDAT)
         WRITE (12,1180) DATNAM(NUMDAT)
         GO TO 900
         END IF
C                                       Done (last char='/')
      IF (CHR.EQ.'/') GO TO 500
      NUMDAT = NUMDAT + 1
      DATNAM(NUMDAT) = '      '
      LNAME = 0
C                                       New line?
      IF (CHR.EQ.'@') GO TO 80
C                                       Next variable
      JPNT = JPNT + 1
      GO TO 50
C                                       Parse values to get types.
 500  HERE = HERE + JPNT - 1
      EPNT = JPNT
      IF (NUMDAT.LE.0) GO TO 999
C                                       NOVAR = no. words declared
         NOVAR = NUMDT
C                                       Now count words DATAed
         NUMDT = 1
C                                       Find '/'
         LP = NEXTNQ (WORK(JPNT:END), '/')
C                                       New line?
         IF (LP.LE.0) GO TO 600
C                                       Find next nonblank
         LIMIT = JPNT + LP
 550     DO 560 LOOP = LIMIT,END
            JPNT = LOOP
            IF (WORK(LOOP:LOOP).NE. ' ') GO TO 570
 560        CONTINUE
C                                       New line
         GO TO 600
C                                       Find next ',', '*' '/' or '@'
 570     IP = NEXTNQ (WORK(JPNT:END), ',')
         IF (IP.LE.0) IP = END + 1
         JP = NEXTNQ (WORK(JPNT:END), '*')
         IF (JP.LE.0) JP = END + 1
         KP = NEXTNQ (WORK(JPNT:END), '@')
         IF (KP.LE.0) KP = END + 1
         LP = NEXTNQ (WORK(JPNT:END), '/')
         IF (LP.EQ.1) GO TO 700
         IF (LP.LE.0) LP = END + 1
C                                       Repeat?
         NODAT = 1
         KPNT = JPNT
         IF ((JP.GT.0) .AND. (JP.LT.MIN (IP, LP))) THEN
            VSTRNG = '      '
            NDIM = JP - 1
            IF (NDIM.LE.0) GO TO 910
            VSTRNG(6-NDIM+1:6) = WORK(JPNT:JPNT+NDIM)
            READ (VSTRNG,1000,ERR=910) NODAT
            KPNT = JPNT + JP
            END IF
C                                       Determine if character
         TYPE = DATTYP(NUMDT)
         IF ((WORK(KPNT:KPNT).EQ.'''') .AND. (WORK(KPNT:KPNT+5).NE.
     *      '''INDE''') .AND. (TYPE.NE.3)) TYPE = 4
C                                       Fill DATTYP
         DATTYP(NUMDT) = TYPE
         NUMDT = NUMDT + 1
         NDATA = NDATA + NODAT
C                                       Next value
         JPNT = JPNT + MIN (IP, LP)
         LIMIT = JPNT
C                                       Done?
         IF (LP.LT.IP) GO TO 700
C                                       End of line?
         IF (JPNT.GE.END) GO TO 600
         GO TO 550
C                                       New line
 600     HERE = HERE + END - EPNT
         IPNT = NEXTNQ (MODULE(HERE+1:HERE+75), '@')
         IF (IPNT.LE.0) IPNT = INDEX (MODULE(HERE+1:HERE+75), '@')
         IPNT = IPNT + 1
         WORK = MODULE(HERE+7:HERE+IPNT-1)
         END = IPNT - 7
         JPNT = 1
         HERE = HERE + 7
         LIMIT = JPNT
         EPNT = 1
         GO TO 550
 700  POINT = HERE + JPNT - EPNT
C                                       Make sure arguments match
      IF (NDATA.NE.NOVAR) THEN
         RECOVE = .FALSE.
         WORK(1:25) = 'Arg-val mismatch'
         GO TO 930
         END IF
      GO TO 999
C                                       can't figure it out - quit.
C                                       Skip to next '/'
 900  IPNT = NEXTNQ (MODULE(HERE:BEGFOR), '/')
      HERE = HERE + IPNT
      RECOVE = .FALSE.
      GO TO 920
C                                       Can I recover ( 1 parameter)
 910  RECOVE = (NUMDAT.EQ.1) .AND. (NUMDT.GT.1)
C                                       Skip to next '/'
 920  IPNT = NEXTNQ (MODULE(HERE+1:BEGFOR), '/') + 1
      HERE = HERE + IPNT
      POINT = HERE
      WORK(1:25) = 'Trouble parsing'
 930  IF (.NOT.RECOVE) THEN
         WRITE (6,1900)
         IPNT = HERE + 500
         CALL LOGER (MODULE, IPNT, HERE, WORK(1:25))
         NUMDAT = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I6)
 1180 FORMAT (' Variable ',A,' DATAed but not declared locally')
 1900 FORMAT (' Trouble parsing DATA statment.')
      END
      INTEGER FUNCTION NEXTNQ (STRING, CHAR)
C-----------------------------------------------------------------------
C   Function to determine next occurance of a character in a string
C   which is not inside quotes. 0 => not found.
C    Inputs:
C     STRING    C   String to be searched
C     CHAR      C   Character to locate
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), CHAR*1
      LOGICAL   QUOTED
      INTEGER   LIMIT, IT
C-----------------------------------------------------------------------
      LIMIT = LEN (STRING)
      QUOTED = .FALSE.
      NEXTNQ = 0
C                                       Is it there at all
      IT = INDEX (STRING(1:LIMIT), CHAR)
      IF (IT.LE.0) GO TO 999
C                                       look for CHAR
 10   CONTINUE
         NEXTNQ = NEXTNQ + 1
         IF (NEXTNQ.GT.LIMIT) GO TO 900
         IF (STRING(NEXTNQ:NEXTNQ).EQ.'''') QUOTED = .NOT.QUOTED
         IF (QUOTED .OR. (STRING(NEXTNQ:NEXTNQ).NE.CHAR)) GO TO 10
      GO TO 999
C                                       Not found
 900  NEXTNQ = 0
C
 999  RETURN
      END
      SUBROUTINE RMDCL (PAGE, MODULE, LMODUL)
C-----------------------------------------------------------------------
C   Routine to remove the declaration of unused variables.
C    Input/Output:
C     PAGE    I  Number of pages in MODULE
C     MODULE  C  Module string, '@' => line break
C     LMODUL  I  Number of valid characters in MODULE (including '@')
C   Input from common
C     BEGDCL       I  Pointer to the first character of the declaration
C     ENDDCL       I  Pointer to the last character of the declaration
C     BEGFOR       I  Pointer to the first character of the executable
C     DCLPOS(3,*)  I  Declaration position pointers wrt BEGDCL
C                     First character, last character, type:
C                     1 = INTEGER, 2 = REAL, 3 = HOLLERITH,
C                     4 = CHARACTER, 5 = LOGICAL, 6 = DOUBLE PRECISION,
C                     7 = COMPLEX
C     NUMDCL       I  Number of entries in DCLPOS
C-----------------------------------------------------------------------
      INTEGER   PAGE
      CHARACTER MODULE(PAGE)*(*)
      INTEGER   LMODUL(PAGE)
      CHARACTER VARNAM*30, CHR*1, BC*1, EC*1, OKCH*13, DCLSTR*100
      INTEGER   LVNAME, START, IPNT, LOOP, LENDCL(7), DCLLOC(3,100),
     *   IDCL, IPAGE, POINT, LIMIT, LIMIT2, ICHR, KPNT, LDSTR, PLEVEL
      INCLUDE 'XEDIT.INC'
      DATA OKCH /' ,(@*)=+-./!:'/
      DATA LENDCL /13,10,15,15,13,22,13/
C-----------------------------------------------------------------------
      IF ((PAGE.LE.0) .OR. (LMODUL(1).LE.0)) GO TO 999
      IF (BEGDCL.GE.ENDDCL) GO TO 999
C                                       Reference declarations to page
      DO 10 LOOP = 1,NUMDCL
         DCLLOC(1,LOOP) = DCLPOS(1,LOOP) + BEGDCL - 1
         DCLLOC(2,LOOP) = DCLPOS(2,LOOP) + BEGDCL - 1
         DCLLOC(3,LOOP) = DCLPOS(3,LOOP)
 10      CONTINUE
      IPAGE = 1
C                                       Loop over declaration statments
      DO 700 IDCL = 1,NUMDCL
C                                       Start position in declaration
C                                       line.
         POINT = DCLLOC(1,IDCL) + LENDCL(DCLLOC(3,IDCL)) + 1
         LIMIT2 = DCLLOC(2,IDCL)
C                                       Find start of next variable
 50         LIMIT = POINT
            IF (POINT.GT.LIMIT2) GO TO 700
            DO 60 LOOP = LIMIT,LIMIT2
               CHR = MODULE(1)(POINT:POINT)
               ICHR = ICHAR (CHR)
C                                       Letter
               IF ((ICHR.GE.ICHA) .AND. (ICHR.LE.ICHZ)) GO TO 100
               POINT = POINT + 1
C                                       new line?
               IF (CHR.EQ.'@') POINT = POINT + 6
C                                       Done with declaration?
               IF (POINT.GT.LIMIT2) GO TO 700
 60            CONTINUE
             GO TO 700
C                                       Find number of characters in
C                                       name.
 100        LVNAME = 0
            VARNAM = '      '
            DO 120 LOOP = 1,30
               CHR = MODULE(1)(POINT+LVNAME:POINT+LVNAME)
               IF (INDEX (OKCH(1:5), CHR).GT.0) GO TO 200
               LVNAME = LVNAME + 1
               VARNAM(LVNAME:LVNAME) = CHR
 120        CONTINUE
C                                       Look for string
C                                       Next dcl if confused.
 200        IF (LVNAME.LE.0) GO TO 700
C                                       Log long names:
            IF (LVNAME.GT.6) THEN
               WRITE (12,1200,ERR=205) VARNAM(1:LVNAME)
               END IF
C                                       Check call sequence
 205        START = 1
 210        IPNT = INDEX (MODULE(1)(START:BEGDCL), VARNAM(1:LVNAME))
            IF  (IPNT.GT.0) THEN
C                                       Verify name
               KPNT = START + IPNT - 1
               BC = MODULE(1)(KPNT-1:KPNT-1)
               EC = MODULE(1)(KPNT+LVNAME:KPNT+LVNAME)
               IF ((INDEX (OKCH, BC).LE.0) .OR. (INDEX (OKCH, EC).LE.0))
     *            THEN
                  START = KPNT + LVNAME
                  GO TO 210
                  END IF
C                                       Found it
                  GO TO 290
               END IF
C                                       Used after declaration?
            START = POINT + LVNAME
            IPAGE = 1
 250        IPNT = INDEX (MODULE(IPAGE)(START:LMODUL(IPAGE)),
     *         VARNAM(1:LVNAME))
C                                       More pages?
            IF ((IPNT.LE.0) .AND. (IPAGE.LT.PAGE)) THEN
                IPAGE = IPAGE + 1
                START = 1
                GO TO 250
                END IF
C                                       Found?
 290        IF (IPNT.GT.0) THEN
C                                       Verify name
               KPNT = START + IPNT - 1
               BC = MODULE(IPAGE)(KPNT-1:KPNT-1)
               EC = MODULE(IPAGE)(KPNT+LVNAME:KPNT+LVNAME)
               IF ((INDEX (OKCH, BC).LE.0) .OR. (INDEX (OKCH, EC).LE.0))
     *            THEN
                  START = KPNT + LVNAME
                  GO TO 250
                  END IF
C                                       Skip rest of declaration
C                                       Find next level 0 ',' or '@'
               LIMIT = POINT
               PLEVEL = 0
               DO 300 LOOP = LIMIT,LIMIT2
                  IF (POINT.GT.LIMIT2) GO TO 700
                  CHR = MODULE(1)(POINT:POINT)
                  IF (CHR.EQ.'(') PLEVEL = PLEVEL + 1
                  IF (CHR.EQ.')') PLEVEL = PLEVEL - 1
                  IF (PLEVEL.EQ.0) THEN
                     IF ((CHR.EQ.',') .OR. (CHR.EQ.'@')) GO TO 50
                     END IF
                  POINT = POINT + 1
C                                       new line?
                  IF (CHR.EQ.'@') POINT = POINT + 6
 300              CONTINUE
C                                       Zap declaration
            ELSE
               CALL PARSEA (MODULE(1)(POINT:LMODUL(1)),
     *            DCLLOC(3,IDCL), MODULE(1)(DCLLOC(1,IDCL):POINT-1),
     *            DCLSTR, LDSTR)
               END IF
            GO TO 50
 700     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT (' Illegal length variable name: ',A)
      END
      SUBROUTINE FNDUSE (PAGE, MODULE, LMODUL, BEGFOR, NKEEP, KEEP,
     *   NARG, ARG)
C-----------------------------------------------------------------------
C   Routine to find if any variables in the Forced declaration are used.
C    Input
C     PAGE    I  Number of pages in MODULE
C     MODULE  C  Module string, '@' => line break
C     LMODUL  I  Number of valid characters in MODULE (including '@')
C     BEGFOR  I  Pointer to the first character of the executable
C     NKEEP   I  Number of elements in KEEP
C     KEEP()  C  Variable names to force declarations if used.
C   Input/Output:
C     NARG    I  Number of elements in ARG
C     ARG()   C  Variables to declare.
C-----------------------------------------------------------------------
      INTEGER   PAGE
      CHARACTER MODULE(PAGE)*(*), KEEP(*)*6, ARG(*)*6
      INTEGER   LMODUL(PAGE), BEGFOR, NKEEP, NARG
      CHARACTER VARNAM*6, CHR*1, BC*1, EC*1, OKCH*13
      INTEGER   LVNAME, START, IPNT, LOOP, IPAGE, IKEEP, ITRIM, KPNT
      DATA OKCH /' ,(@*)=+-./!:'/
C-----------------------------------------------------------------------
      IF ((PAGE.LE.0) .OR. (LMODUL(1).LE.0)) GO TO 999
      IF (BEGFOR.GE.LMODUL(PAGE)) GO TO 999
C                                       Loop over declaration statments
      DO 700 IKEEP = 1,NKEEP
         VARNAM = KEEP(IKEEP)
         LVNAME = ITRIM (VARNAM)
C                                       Have it already?
         DO 200 LOOP = 1,NARG
            IF (ARG(LOOP).EQ.VARNAM) GO TO 700
 200        CONTINUE
C                                       Look for variable
         START = BEGFOR
         DO 500 IPAGE = 1,PAGE
 100        IPNT = INDEX (MODULE(IPAGE)(START:LMODUL(IPAGE)),
     *         VARNAM(1:LVNAME))
            IF (IPNT.GT.0) THEN
C                                       Verify
               KPNT = START + IPNT - 1
               BC = MODULE(IPAGE)(KPNT-1:KPNT-1)
               EC = MODULE(IPAGE)(KPNT+LVNAME:KPNT+LVNAME)
               IF ((INDEX (OKCH, BC).LE.0) .OR. (INDEX (OKCH, EC).LE.0))
     *            THEN
C                                       Invalid name
                  START = KPNT + LVNAME
C                                       Try again
                  GO TO 100
                  END IF
C                                       Save it
               NARG = NARG + 1
               ARG(NARG) = VARNAM
               END IF
            START = 1
 500        CONTINUE
 700     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CHL2U (STRING, LSTR)
C-----------------------------------------------------------------------
C   Converts FORTRAN in STRING from lower to upper case.  No
C   translation is done to comment lines except to be sure that the
C   first character is 'C'.  No translation is done to values inside
C   quotes or in inline comments.
C    Input:
C     LSTR     I  Length of STRING
C    Input/Output:
C     STRING   C  String containing a line of Fortran.
C-----------------------------------------------------------------------
      INTEGER   LSTR
      CHARACTER STRING*(*)
      INTEGER   LOWA, UPA, LOOP, ICH
      LOGICAL   QUOTE
      CHARACTER CH*1
C-----------------------------------------------------------------------
C                                       Comment line?
      IF (STRING(1:1).EQ.'c') STRING(1:1) = 'C'
      IF (STRING(1:1).EQ.'*') STRING(1:1) = 'C'
      IF (STRING(1:1).EQ.'C') GO TO 999
C                                       Prepare to translate
      LOWA = ICHAR ('a')
      UPA  = ICHAR ('A')
      QUOTE = .FALSE.
C                                       Translate
      DO 100 LOOP = 1,LSTR
         CH = STRING(LOOP:LOOP)
C                                       Bail out if inline comment
         IF (CH.EQ.'!') GO TO 999
         IF (CH.EQ.'''') QUOTE = .NOT.QUOTE
C                                       Ignore things in quotes
         IF (.NOT.QUOTE) THEN
C                                       Translate
            ICH = ICHAR (CH) - LOWA
            IF ((ICH.GE.0) .AND. (ICH.LE.25)) STRING(LOOP:LOOP) =
     *         CHAR (ICH+UPA)
            END IF
 100     CONTINUE
C
 999  RETURN
      END
