      SUBROUTINE IMGERR (OPCODE, FDVEC, IBUFF, IPNT, EBUFF, EPNT, IERR)
C-----------------------------------------------------------------------
C! Reads rows of an image and the associated error image.
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   Reads or writes image pixel values and their associated errors.  The
C   axis containing the pixel errors should be labeled "ERROR" and
C   should not the the first axis in the data cube.  If the "ERROR" axis
C   is absent then blanked values (FBLANK in DDCH.INC) will be returned
C   as the error values for reading and nothing is done for writing.
C      Most of the necessary information needed to define the file,
C   windows etc. is passed through the array FDVEC.  A standard include
C   file IMGERR.INC defines pointers into this array and it is advised
C   to use these pointers rather than hardcoded subscripts.
C      Mixed reads and writes are not supported using the same LUNs and
C   buffers but IMGERR may be used with separate LUNs, buffers and FDVEC
C   to read and write the same file.
C      IMGERR keeps track of the max. and min. values and the use of
C   blanking when OPCODE='WRIT'.  The version of the catalog header is
C   read from and rewritten to disk when the file is closed.
C      IMGERR will add 'READ' or 'WRIT' status to the catalog for the
C   associated file and clear it when the file is closed.  Note that
C   this requires ignoring previous catalog status codes so it is the
C   responsibility of the calling task to be sure that the operation of
C   this routine does not conflict with useage of the files involved by
C   other tasks.
C   Usage notes:
C     1) Zero fill FDVEC before filling in relevant values.
C     2) Opening the file.  If IMGERR determines that the file is not
C        open it will do so.   Once the file is open the file
C        descriptor vector FDVEC must be used in each call.  No explicit
C        open call is needed.
C     3) Initialization.  IMGERR initializes the I/O using the values in
C        FDVEC when it opens the file.
C     4) Closing the file.  The file may be closed with a call with
C        opcode 'CLOS'.  If OPCODE is 'WRIT' then the buffers are
C        flushed before the file is closed.
C     5) Selecting a subset of the first axis is not allowed for
C        OPCODE='WRIT'.
C     6) Buffers will be flushed and files closed after the last call on
C        a specified subimage.
C     7) For OPCODE='WRIT' the initial values of IPNT and EPNT are 1.
C   Inputs:
C      OPCODE C*4     Operation code: 'READ','WRIT','CLOS'
C   Input/Output:
C      FDVEC  I(80)   File descriptor vector; indices are the pointers
C                     from IMGERR.INC.
C                     FDDISK = Disk number of file.
C                     FDCNO  = Catalog slot number of file
C                     FDLUNI = LUN to use for image, set before first
C                              call.
C                     FDLUNE = LUN to use for errors, set before first
C                              call.
C                     FDBLC = BLC of window in the image. 0's => all
C                             non-error axes. (array of 7 values)
C                     FDTRC = TRC of window in the image. 0's => all
C                             non-error axes. (array of 7 values)
C                    FDSZIB = Size of IBUFF in words.
C                    FDSZEB = Size of EBUFF in words.
C                The following are used by IMGERR:
C                    FDFTI = FTAB pointer for image I/O. > 0 =>file open
C                    FDFTE = FTAB pointer for error I/O. > 0 =>file open
C                    FDERR = Subroutine error return code.
C                    FDPOS = pointer in image to word in image buffer
C                            pointed to by IPNT. (array of 7 values)
C                    FDNLEF = Number of rows left in current plane.
C                    FDRW   = 0 if read, 1 if write.
C                    FDISER = 0 if error plane exists.
C                    FDIMSI = Image size (array of 7 values)
C                    FDIMPT = Plane pointer (array of 7 values)
C                    FDERPL = Error plane order number
C                    FDDCAT = 1 => clear catalog flag at end
C                    FDMAX  = Image max. value as REAL
C                    FDMIN  = Image min. value as REAL
C                    FDBLK  = 1 if magic value blanking used,
C                             0 otherwise.
C      IBUFF  R(*)   Buffer for Image I/O must be large enough for the
C                    largest transfer rounded up to the next larger
C                    number of disk blocks.
C      EBUFF  R(*)   Buffer for Error I/O must be large enough for the
C                    largest transfer rounded up to the next larger
C                    number of disk blocks.
C   Outputs:
C      IPNT   I      The location in IBUFF of the start of the next
C                    record. Before the first write call this should
C                    be set to 1 to determine where to start filling
C                    IBUFF.
C      EPNT   I      The location in EBUFF of the start of the next
C                    record. Before the first write call this should
C                    be set to 1 to determine where to start filling
C                    EBUFF.
C     IERR    I      Error return: 0 => ok,
C                                 -1 => Finished at end of call
C                                  1 => Input error
C                                  2 => ZOPEN error
C                                  3 => CATIO error
C                                  4 => ZOPEN error
C                                  5 => MINIT error
C                                  6 => MDISK error
C                                  7 => ZCLOSE error
C                                  8 => CATDIR error
C                                  9 => COMOFF error
C                                  Subroutine error codes are returned
C                                  in FDVEC(32).
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   FDVEC(80), IPNT, EPNT, IERR
      REAL      IBUFF(*), EBUFF(*)
      CHARACTER TYPE*2, NAME*12, CLASS*6, STATUS*4, FNAME*48,
     *   CLRCOD(2)*4, OPTMP*4
      INTEGER   CATBLK(256), WBUFF(256), NCOPY, LOCERR, BOI, BOE,
     *   WIN(4), LOOP, POS(7), SEQ, JERR, MSGSAV, IMAXV, IMINV, NCHK
      REAL      CATR(256), MAXV, MINV
      LOGICAL   NEXT, ISBLNK
      HOLLERITH CATH(256)
      INCLUDE 'INCS:IMGERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH)
      EQUIVALENCE (IMAXV, MAXV),  (IMINV, MINV)
      DATA CLRCOD /'CLRD','CLWR'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      IF ((OPCODE.NE.'READ') .AND. (OPCODE.NE.'WRIT') .AND.
     *   (OPCODE.NE.'CLOS')) THEN
         IERR = 1
         MSGTXT = 'IMGERR: UNKNOWN OPCODE =' // OPCODE
         GO TO 990
         END IF
C                                       Close?
      IF (OPCODE.EQ.'CLOS') GO TO 800
C                                       Need to Open?  Check if both
C                                       files are already opened.
      IF ((FDVEC(FDFTI).LE.0) .OR. (FDVEC(FDFTE).LE.0)) THEN
C                                       Get CATBLK
         STATUS = OPCODE
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL CATIO ('READ', FDVEC(FDDISK), FDVEC(FDCNO), CATBLK,
     *      STATUS, WBUFF, FDVEC(FDERR))
         MSGSUP = MSGSAV
C                                       Can I unflag this entry later?
         IF (FDVEC(FDERR).EQ.5) THEN
C                                       No
            FDVEC(FDCCAT) = 0
C                                       Call CATIO again, the first time
C                                       didn't work.
            STATUS = 'REST'
            MSGSAV = MSGSUP
            MSGSUP = 32000
            CALL CATIO ('READ', FDVEC(FDDISK), FDVEC(FDCNO), CATBLK,
     *         STATUS, WBUFF, FDVEC(FDERR))
            MSGSUP = MSGSAV
         ELSE
C                                       Yes
            FDVEC(FDCCAT) = 1
            END IF
C                                       Must ignore warnings about
C                                       catalog flags.
         IF (((FDVEC(FDERR).GT.0).AND.(FDVEC(FDERR).LE.4)) .OR.
     *      (FDVEC(FDERR).GE.10)) THEN
            IERR = 3
            WRITE (MSGTXT,1000) FDVEC(FDERR)
            GO TO 990
            END IF
C                                       Must be type 'MA' or 'SC'
         CALL H2CHR (2, KHPTYO, CATH(KHPTY), TYPE)
         IF ((TYPE.NE.'MA') .AND. (TYPE.NE.'SC')) THEN
            IERR = 1
            MSGTXT = 'IMGERR: INPUT FILE NOT AN IMAGE OR SCRATCH FILE'
            GO TO 990
            END IF
C                                       Save info - image size
         CALL COPY (7, CATBLK(KINAX), FDVEC(FDIMSI))
C                                       Does 'ERROR' axis exist?
         CALL AXEFND (8, 'ERROR   ', CATBLK(KIDIM), CATH(KHCTP), LOCERR,
     *      JERR)
         IF ((JERR.EQ.0) .AND. (LOCERR.GE.0)) THEN
            IF (LOCERR.LE.1) THEN
C                                       Error axis cannot be the first
C                                       two.
               IERR = 1
               MSGTXT = 'IMGERR: ERROR AXIS CANNOT BE THE FIRST TWO'
               GO TO 990
               END IF
C                                       Save error plane info
            FDVEC(FDISER) = 1
            FDVEC(FDERPL) = LOCERR
         ELSE
C                                       No error plane
            FDVEC(FDISER) = 0
            END IF
C                                       Set default window.
         DO 200 LOOP = 1,7
            FDVEC(FDIMSI+LOOP-1) = MAX (FDVEC(FDIMSI+LOOP-1), 1)
            FDVEC(FDBLC+LOOP-1) = MAX (FDVEC(FDBLC+LOOP-1), 1)
            IF (FDVEC(FDTRC+LOOP-1).LE.0) FDVEC(FDTRC+LOOP-1) =
     *         FDVEC(FDIMSI+LOOP-1)
C                                       Init plane pointers.
            FDVEC(FDIMPT+LOOP-1) = FDVEC(FDBLC+LOOP-1)
 200        CONTINUE
C                                       Max/min and blanking
C                                       Note use of EQUIVALENCE
         FDVEC(FDBLK) = 0
         IF (FDVEC(FDBLC+2).GT.1) THEN
C                                       Use old values if starting in
C                                       middle of image.
            MAXV = CATR(KRDMX)
            MINV = CATR(KRDMN)
            IF (CATR(KRBLK).EQ.FBLANK) FDVEC(FDBLK) = 1
         ELSE
C                                       Init.
            MAXV = -1.0E20
            MINV = 1.0E20
            END IF
C                                       Save values (need only for
C                                       'WRIT')
         FDVEC(FDMAX) = IMAXV
         FDVEC(FDMIN) = IMINV
C                                       Open files as necessary
         IF (FDVEC(FDFTI).LE.0) THEN
C                                       Image file
            CALL ZPHFIL (TYPE, FDVEC(FDDISK), FDVEC(FDCNO), 1, FNAME,
     *         FDVEC(FDERR))
            CALL ZOPEN (FDVEC(FDLUNI), FDVEC(FDFTI), FDVEC(FDDISK),
     *         FNAME, .TRUE., .FALSE., .TRUE., FDVEC(FDERR))
            IF (FDVEC(FDERR).GT.0) THEN
               IERR = 4
               WRITE (MSGTXT,1001) FDVEC(FDERR), 'IMAGE'
               GO TO 990
               END IF
            END IF
         IF ((FDVEC(FDISER).EQ.1) .AND. (FDVEC(FDFTE).LE.0)) THEN
C                                       Error plane exists
            CALL ZPHFIL (TYPE, FDVEC(FDDISK), FDVEC(FDCNO), 1, FNAME,
     *         FDVEC(FDERR))
            CALL ZOPEN (FDVEC(FDLUNE), FDVEC(FDFTE), FDVEC(FDDISK),
     *         FNAME, .TRUE., .FALSE., .TRUE., FDVEC(FDERR))
            IF (FDVEC(FDERR).GT.0) THEN
               IERR = 4
               WRITE (MSGTXT,1001) FDVEC(FDERR), 'ERROR'
               GO TO 990
               END IF
            END IF
         IF (FDVEC(FDFTE).LE.0) FDVEC(FDFTE) = 1
C                                       Set values in FDVEC
C                                       Number of rows left - force init
         FDVEC(FDNLEF) = 0
C                                       Subroutine error
         FDVEC(FDERR) = 0
C                                       Read/write flag
         FDVEC(FDRW) = 0
         IF (OPCODE.EQ.'WRIT') FDVEC(FDRW) = 1
         END IF
C----------------------------------------------------------------------
C                                       Time to init files?
      IF (FDVEC(FDNLEF).LE.0) THEN
C                                       Done if pointer on 3rd axis .le.
C                                       0.
         IF (FDVEC(FDIMPT+2).LE.0) GO TO 800
C                                       Set window
         WIN(1) = FDVEC(FDBLC)
         WIN(2) = FDVEC(FDBLC+1)
         WIN(3) = FDVEC(FDTRC)
         WIN(4) = FDVEC(FDTRC+1)
C                                       Block offset for image
         CALL COMOFF (KICTPN, FDVEC(FDIMSI), FDVEC(FDIMPT+2), BOI,
     *      FDVEC(FDERR))
         BOI = BOI + 1
         IF (FDVEC(FDERR).NE.0) THEN
            IERR = 9
            MSGTXT = 'IMGERR: ERROR IN COMOFF COMPUTING BLOCK OFFSET'
            GO TO 990
            END IF
C                                       Init I/O
         CALL MINIT (OPCODE, FDVEC(FDLUNI), FDVEC(FDFTI), FDVEC(FDIMSI),
     *      FDVEC(FDIMSI+1), WIN, IBUFF, FDVEC(FDSZIB), BOI,
     *      FDVEC(FDERR))
         IF (FDVEC(FDERR).NE.0) THEN
            IERR = 5
            WRITE (MSGTXT,1002) FDVEC(FDERR) ,'IMAGE'
            GO TO 990
            END IF
C                                       Number of rows to read
         FDVEC(FDNLEF) = ABS (WIN(4) - WIN(2)) + 1
C                                       Error plane?
         IF (FDVEC(FDISER).EQ.1) THEN
C                                       Block offset for error
            CALL COPY (7, FDVEC(FDIMPT), POS)
C                                       Errors are second pixel on Error
C                                       axis.
            POS(FDVEC(FDERPL)+1) = 2
            CALL COMOFF (KICTPN, FDVEC(FDIMSI), POS(3), BOE,
     *         FDVEC(FDERR))
            BOE = BOE + 1
            IF (FDVEC(FDERR).NE.0) THEN
               IERR = 9
               MSGTXT = 'IMGERR: ERROR IN COMOFF COMPUTING BLOCK OFFSET'
               GO TO 990
               END IF
C                                       Init I/O
            CALL MINIT (OPCODE, FDVEC(FDLUNE), FDVEC(FDFTE),
     *         FDVEC(FDIMSI), FDVEC(FDIMSI+1), WIN, EBUFF,
     *         FDVEC(FDSZEB), BOE, FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) THEN
               IERR = 5
               WRITE (MSGTXT,1002) FDVEC(FDERR) ,'IMAGE'
               GO TO 990
               END IF
            END IF
C                                       Set plane pointers for next
C                                       plane.
         NEXT = .TRUE.
         DO 300 LOOP = 3,7
            IF (NEXT .AND. (LOOP.NE.(FDVEC(FDERPL)+1))) THEN
               FDVEC(FDIMPT+LOOP-1) = FDVEC(FDIMPT+LOOP-1) + 1
               NEXT = FDVEC(FDIMPT+LOOP-1) .GT. FDVEC(FDTRC+LOOP-1)
               IF (NEXT) FDVEC(FDIMPT+LOOP-1) = FDVEC(FDBLC+LOOP-1)
               END IF
 300        CONTINUE
C                                       If NEXT still true then done
         IF (NEXT) FDVEC(FDIMPT+2) = -1
C                                       Image is pixel 1 on ERROR axis
         FDVEC(FDIMPT+FDVEC(FDERPL)) = 1
         FDVEC(FDIMPT+1) = FDVEC(FDBLC+1)
C                                       Dummy first writes
         IF (FDVEC(FDRW).EQ.1) THEN
            CALL MDISK ('WRIT', FDVEC(FDLUNI), FDVEC(FDFTI), IBUFF,
     *         IPNT, FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) THEN
               IERR = 6
               WRITE (MSGTXT,1003) FDVEC(FDERR), 'WRIT', 'IMAGE'
               GO TO 990
               END IF
C                                       Error plane?
            IF (FDVEC(FDISER).EQ.1) THEN
               CALL MDISK ('WRIT', FDVEC(FDLUNE), FDVEC(FDFTE),
     *            EBUFF, EPNT, FDVEC(FDERR))
               IF (FDVEC(FDERR).NE.0) THEN
                  IERR = 6
                  WRITE (MSGTXT,1003) FDVEC(FDERR), 'WRIT', 'ERROR'
                  GO TO 990
                  END IF
               END IF
            END IF
         END IF
C-----------------------------------------------------------------------
C                                       Do I/O operations.
C                                       Check OPCODE
      IF (((OPCODE.EQ.'READ').AND.(FDVEC(FDRW).NE.0)) .OR.
     *   ((OPCODE.EQ.'WRIT').AND.(FDVEC(FDRW).NE.1))) THEN
         IERR = 1
         MSGTXT = 'OPCODE INCOMPATIBLE WITH PREVIOUS USEAGE'
         GO TO 990
         END IF
      OPTMP = OPCODE
C                                       Max, min etc for WRIT
      IF (FDVEC(FDRW).EQ.1) THEN
         IMAXV = FDVEC(FDMAX)
         IMINV = FDVEC(FDMIN)
         ISBLNK = FDVEC(FDBLK).EQ.1
         END IF
C                                       Flush buffers?
      IF ((FDVEC(FDNLEF).LE.1) .AND. (OPCODE.EQ.'WRIT')) THEN
         OPTMP = 'FINI'
         END IF
C                                       I/O to image
      CALL MDISK (OPTMP, FDVEC(FDLUNI), FDVEC(FDFTI), IBUFF, IPNT,
     *   FDVEC(FDERR))
      IF (FDVEC(FDERR).NE.0) THEN
         IERR = 6
         WRITE (MSGTXT,1003) FDVEC(FDERR), OPTMP, 'IMAGE'
         GO TO 990
         END IF
C                                       Max/min etc for 'WRIT'
      IF (FDVEC(FDRW).EQ.1) THEN
         NCHK = FDVEC(FDTRC) - FDVEC(FDBLC) + 1
         DO 500 LOOP = 1,NCHK
            MAXV = MAX (MAXV, IBUFF(IPNT+LOOP-1))
            MINV = MIN (MINV, IBUFF(IPNT+LOOP-1))
            ISBLNK = ISBLNK .OR. (IBUFF(IPNT+LOOP-1).EQ.FBLANK)
 500        CONTINUE
         END IF
C                                       Error plane?
      IF (FDVEC(FDISER).EQ.1) THEN
         CALL MDISK (OPTMP, FDVEC(FDLUNE), FDVEC(FDFTE), EBUFF,
     *      EPNT, FDVEC(FDERR))
         IF (FDVEC(FDERR).NE.0) THEN
            IERR = 6
            WRITE (MSGTXT,1003) FDVEC(FDERR), OPTMP, 'ERROR'
            GO TO 990
            END IF
C                                       Max/min etc for 'WRIT'
        IF (FDVEC(FDRW).EQ.1) THEN
           NCHK = FDVEC(FDTRC) - FDVEC(FDBLC) + 1
           DO 510 LOOP = 1,NCHK
              MAXV = MAX (MAXV, EBUFF(EPNT+LOOP-1))
              MINV = MIN (MINV, EBUFF(EPNT+LOOP-1))
              ISBLNK = ISBLNK .OR. (EBUFF(EPNT+LOOP-1).EQ.FBLANK)
 510          CONTINUE
           END IF
C                                       No error plane, blank fill on
C                                       read
      ELSE IF (FDVEC(FDRW).EQ.0) THEN
         NCOPY = FDVEC(FDTRC) - FDVEC(FDBLC) + 1
         CALL RFILL (NCOPY, FBLANK, EBUFF)
         EPNT = 1
         END IF
C                                       Decrement line count
      FDVEC(FDNLEF) = FDVEC(FDNLEF) - 1
      FDVEC(FDIMPT+1) = FDVEC(FDIMPT+1) + 1
C                                       Save max, min etc for WRIT
      IF (FDVEC(FDRW).EQ.1) THEN
         FDVEC(FDMAX) = IMAXV
         FDVEC(FDMIN) = IMINV
         FDVEC(FDBLK) = 0
         IF (ISBLNK) FDVEC(FDBLK) = 1
         END IF
C                                       Reset pointers if flushed
C                                       buffers.
      IF (OPTMP.EQ.'FINI') THEN
         IPNT = 1
         EPNT = 1
         END IF
C                                       Close if done
      IF ((FDVEC(FDNLEF).LE.0) .AND. (FDVEC(FDIMPT+2).LE.0))
     *   GO TO 800
      GO TO 999
C----------------------------------------------------------------------
C                                       Close files etc.
 800  CALL ZCLOSE (FDVEC(FDLUNI), FDVEC(FDFTI), FDVEC(FDERR))
      IF (FDVEC(FDERR).NE.0) THEN
         IERR = 7
         WRITE (MSGTXT,1801) FDVEC(FDERR)
         GO TO 990
         END IF
C                                       Update CATBLK on 'WRIT'
      IF (OPCODE.EQ.'WRIT') THEN
         STATUS = 'REST'
         MSGSAV = MSGSUP
         MSGSUP = 32000
C                                       Read from disk
         CALL CATIO ('READ', FDVEC(FDDISK), FDVEC(FDCNO), CATBLK,
     *      STATUS, WBUFF, FDVEC(FDERR))
         MSGSUP = MSGSAV
C                                       Must ignore warnings about
C                                       catalog flags.
         IF (((FDVEC(FDERR).GT.0).AND.(FDVEC(FDERR).LE.4)) .OR.
     *      (FDVEC(FDERR).GE.10)) THEN
            IERR = 3
            WRITE (MSGTXT,1000) FDVEC(FDERR)
            GO TO 990
            END IF
C                                       Save info
         IMAXV = FDVEC(FDMAX)
         IMINV = FDVEC(FDMIN)
         CATR(KRDMX) = MAXV
         CATR(KRDMN) = MINV
         CATR(KRBLK) = 0.0
         IF (FDVEC(FDBLK).EQ.1) CATR(KRBLK) = FBLANK
C                                       Save CATBLK
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL CATIO ('UPDT', FDVEC(FDDISK), FDVEC(FDCNO), CATBLK,
     *      STATUS, WBUFF, FDVEC(FDERR))
         MSGSUP = MSGSAV
         IF (((FDVEC(FDERR).GT.0).AND.(FDVEC(FDERR).LE.4)) .OR.
     *      (FDVEC(FDERR).GE.10)) THEN
            IERR = 3
            WRITE (MSGTXT,1000) FDVEC(FDERR)
            GO TO 990
            END IF
         END IF
C                                       Clear catalog status
       IF (FDVEC(FDCCAT).EQ.1)
     *   CALL CATDIR ('CSTA', FDVEC(FDDISK), FDVEC(FDCNO), NAME, CLASS,
     *   SEQ, TYPE, NLUSER, CLRCOD(FDVEC(FDRW)+1), WBUFF, FDVEC(FDERR))
      IF (FDVEC(FDERR).NE.0) THEN
C                                       Ignore Errors here
         IERR = 0
         WRITE (MSGTXT,1802) FDVEC(FDERR)
         GO TO 990
         END IF
C                                       Close error file
      IF (FDVEC(FDISER).EQ.1) THEN
         CALL ZCLOSE (FDVEC(FDLUNE), FDVEC(FDFTE), FDVEC(FDERR))
         IF (FDVEC(FDERR).NE.0) THEN
            IERR = 7
            WRITE (MSGTXT,1801) FDVEC(FDERR)
            GO TO 990
            END IF
         END IF
C                                       Clear FDVEC
      FDVEC(FDFTI) = 0
      FDVEC(FDFTE) = 0
C                                       Finished
      IERR = -1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMGERR: CATIO ERROR ',I3,' READING CATBLK')
 1001 FORMAT ('IMGERR: ZOPEN ERROR ',I3,' OPENING ',A,' FILE')
 1002 FORMAT ('IMGERR: MINIT ERROR ',I3,' OPENING ',A,' FILE')
 1003 FORMAT ('IMGERR: MDISK ERROR ',I3,1X,A,'ING ',A,' FILE')
 1801 FORMAT ('IMGERR: ZCLOSE ERROR ',I3)
 1802 FORMAT ('IMGERR: CATDIR ERROR ',I3)
      END
