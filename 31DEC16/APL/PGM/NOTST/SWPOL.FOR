LOCAL INCLUDE 'SWPAN.INC'
C                                       Local include for SWPOL
C                                       Describes the antennas to be
C                                       swapped.
C
C   INCLUD      L      See description of NANT below
C   ANTENS      I(50)  List of antennae (all entries from 1 to NANT are
C                      positive
C   NANT        I      Number of antennae in list. If NANT = 0 then all
C                      polarizations are swapped for all antennae,
C                      otherwise only antennae in the list are swapped
C                      if INCLUD is .TRUE.; if INCLUD is .FALSE. then
C                      all antennae except those in the list are swapped
C   SUBARR      I      Subarray number to process
C
      LOGICAL   INCLUD
      INTEGER   ANTENS(50), NANT, SUBARR
C
      COMMON /SWPANT/ INCLUD, ANTENS, NANT, SUBARR
C                                       End of local include SWPAN.INC
LOCAL END
LOCAL INCLUDE 'SWPFL.INC'
C                                       Local include for SWPOL
C                                       Contains the input and output
C                                       file specifications for SWPOL
C
C     INNAME      C*12      Input file name (name)
C     INCLAS      C*6       Input file name (class)
C     INSEQ       I         Input file name (sequence number)
C     INDISK      I         Input file disk number
C     INCNO       I         Input file catalogue number
C     OUTNAM      C*12      Output file name (name)
C     OUTCLS      C*6       Output file name (class)
C     OUTSEQ      I         Output file name (sequence number)
C     OUTDSK      I         Output file disk number
C     OUTCNO      I         Output file catalogue number
C
      CHARACTER INNAME*12, INCLAS*6, OUTNAM*12, OUTCLS*6
      INTEGER   INSEQ, INDISK, INCNO, OUTSEQ, OUTDSK, OUTCNO
C
      COMMON /SWPFCH/ INNAME, INCLAS, OUTNAM, OUTCLS
      COMMON /SWPFNM/ INSEQ, INDISK, INCNO, OUTSEQ, OUTDSK, OUTCNO
C                                       End of local include SWPFL
LOCAL END
LOCAL INCLUDE 'SWPIO.INC'
C                                       Local include for SWPOL.
C                                       Define variables required by
C                                       SWPOL's I/O operations.
C
C   INLUN        I         Local unit number for input file (constant)
C   OUTLUN       I         Local unit number for output file (constant)
C   IFIND        I         FTAB index for input file (initialised by
C                          INITIO)
C   OFIND        I         FTAB index for output file (initialised by
C                          INITIO)
C   LBUFF        I         Length of I/O buffers in words (constant)
C   IBUFF        R(LBUFF)  Input buffer
C   OBUFF        R(LBUFF)  Output buffer
C   NIO          I         Number of visibilities in input buffer
C                          (modified by RDVIS)
C   IBIND        I         Index of start of first visibility in input
C                          buffer (may be modified by RDVIS)
C   OBIND        I         Index of start of first visibility in ouput
C                          buffer
C   VISNO        I         Number of visibility in input buffer
C                          (1 <= VISNO <= NIO) currently being processed
C                          (modified by RDVIS)
C   ILOCWT       I         Offset of WEIGHT from beginning of random
C                          parameters if data are compressed; a negative
C                          number if data are not compressed
C                          (initialised by INITIO; ILOCWT < NRPARM)
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   INLUN, OUTLUN, CATOLD(256)
      PARAMETER (INLUN = 16, OUTLUN = 17)
      INTEGER   IFIND, OFIND, NIO, IBIND, OBIND, VISNO, ILOCWT
      REAL      IBUFF(UVBFSS), OBUFF(UVBFSS), XCENT, DIFPIX
      DOUBLE PRECISION UVSCAL
      COMMON    /SWPIO/ CATOLD, UVSCAL, XCENT, IFIND, OFIND, IBUFF,
     *   OBUFF, NIO, IBIND, OBIND, VISNO, ILOCWT, DIFPIX
C                                       End of local include SWPIO.INC
LOCAL END
      PROGRAM SWPOL
C-----------------------------------------------------------------------
C! Swap polarizations for designated antennae
C# UV UV-Util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000-2001, 2008, 2013-2015
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
C   Swaps linear or circular polarizations for a selected subset of
C   antennae. The input data must contain one of the following
C   combinations of polarizations: (RR, LL), (RR, LL, RL, LR), (XX, YY)
C   or (XX, YY, XY, YX). In the unlikely event that the data has both
C   parallel hands but only one cross hand, the data must be mangled to
C   remove the cross hand or add the other before attempting to swap
C   polarizations.
C
C   Inputs:
C     INNAME       Input file name (name)
C     INCLASS      Input file name (class)
C     INSEQ        Input file name (sequence number)
C     INDISK       Input file disk number
C     OUTNAME      Output file name (name)
C     OUTCLASS     Output file name (class)
C     OUTSEQ       Output file name (sequence number)
C     OUTDISK      Output file disk number
C     ANTENNAS     Antenna list
C     SUBARR       Subarray number
C     DOCALIB      Swap calibration info?
C-----------------------------------------------------------------------
      INTEGER   IERR, SCRTCH(256)
      CHARACTER PRGM*6
      LOGICAL   DOCAL(2)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'SWPAN.INC'
      INCLUDE 'SWPFL.INC'
      INCLUDE 'SWPIO.INC'
C
      PARAMETER (PRGM = 'SWPOL ')
C-----------------------------------------------------------------------
C                                       Get input parameters, initialize
C                                       commons and check input file:
      CALL SWINIT (PRGM, DOCAL, IERR)
C                                       Swap polarizations:
      IF (IERR.EQ.0) CALL SWMAIN (IERR)
C                                       Flush I/O buffers
      IF (IERR.EQ.0) CALL ENDUV (DOCAL, IERR)
C                                       Copy and update history and copy
C                                       tables:
      IF (IERR.EQ.0) CALL SWPHIS (DOCAL(1), IERR)
C                                       Close down:
      CALL DIE (IERR, SCRTCH)
C
      STOP
      END
      SUBROUTINE SWINIT (PRGM, DOCAL, IRET)
C-----------------------------------------------------------------------
C   Initialize AIPS COMMONs and COMMON blocks in local includes; get
C   inputs; open files and check input file for suitability. The
C   polarizations should correspond to one of (RR, LL), (XX, YY)
C   (RR, LL, RL, LR) or (XX, YY, XY, YX) and the fastest varying
C   visibility axis should be the complex axis.
C
C   Input:
C     PRGM    C*6     Program name
C
C   Output:
C     DOCAL   L       .TRUE. if calibration information is to be
C                     swapped, .FALSE. otherwise
C     IRET    I       Return status: 0 -> OK
C                                    1 -> error reading inputs
C                                    2 -> error opening files
C                                    3 -> bad input file
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      LOGICAL   DOCAL(2)
      INTEGER   IRET
C
C   Local variables:
C     NSUBAR   I          Number of subarrays in this data set.
C
C   Variables in /INPARM/ provide temporary storage for POPS adverbs.
C     NPARM    I          Size of adverb list (constant)
C
      INTEGER   NSUBAR, SCRTCH(256), NPARM, I, J, IERR
      LOGICAL   FOUND
      HOLLERITH XINNAM(3), XINCLS(2), XOUTNM(3), XOUTCL(2)
      REAL      XINSEQ, XINDSK, XOUTSQ, XOUTDS, XANT(50), XSUBAR,
     *   XDOCAL, XDOTAB, DOCENT
      PARAMETER (NPARM = 68)
C
      COMMON /INPARM/ XINNAM, XINCLS, XINSEQ, XINDSK, XOUTNM, XOUTCL,
     *   XOUTSQ, XOUTDS, XANT, XSUBAR, XDOCAL, XDOTAB, DOCENT
C
      INCLUDE 'SWPAN.INC'
      INCLUDE 'SWPFL.INC'
      INCLUDE 'SWPIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Initialize AIPS COMMONs:
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
C                                       Get input adverbs:
      CALL GTPARM (PRGM, NPARM, RQUICK, XINNAM, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT, 1000) IERR
            CALL MSGWRT (8)
            CALL RELPOP (1, SCRTCH, IERR)
            END IF
         IRET = 1
         GO TO 999
         END IF
C                                       Restart AIPS if requested:
      IF (RQUICK)
     *      CALL RELPOP (0, SCRTCH, IERR)
C                                       Read filenames:
      CALL H2CHR (12, 1, XINNAM, INNAME)
      CALL H2CHR (6, 1, XINCLS, INCLAS)
      INSEQ = NINT (XINSEQ)
      INDISK = NINT (XINDSK)
      CALL H2CHR (12, 1, XOUTNM, OUTNAM)
      CALL H2CHR (6, 1, XOUTCL, OUTCLS)
      OUTSEQ = NINT (XOUTSQ)
      OUTDSK = NINT (XOUTDS)
      XCENT = DOCENT
C                                       Open files and initialize I/O:
      CALL INITIO (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (8)
         IRET = 2
         GO TO 999
         END IF
C                                       Process antennas adverb:
      NANT = 0
      INCLUD = .TRUE.
      DO 20 I = 1, 50
         IF (ABS (NINT (XANT(I))).GT.0) THEN
            IF (NINT (XANT(I)) .LT. 0)
     *         INCLUD = .FALSE.
C                                       Check whether the current
C                                       antenna is already in the list:
            FOUND = .FALSE.
            J = 0
 10         IF ((J.NE.NANT) .AND. (.NOT.FOUND)) THEN
               J = J + 1
               IF (ABS (NINT (XANT(I))).EQ.ANTENS(J))
     *            FOUND = .TRUE.
               GO TO 10
               END IF
            IF (.NOT.FOUND) THEN
               NANT = NANT + 1
               ANTENS(NANT) = ABS (NINT (XANT(I)))
               END IF
            END IF
 20      CONTINUE
C                                       Process SUBARRAY adverb:
      CALL FNDEXT ('AN', CATBLK, NSUBAR)
      SUBARR = MAX (1, MIN (NINT (XSUBAR), NSUBAR))
C                                       Process DOCAL adverb:
      DOCAL(1) = XDOCAL.GE.0.0
      DOCAL(2) = XDOTAB.GE.0.0
C                                       Check axis number of complex
C                                       axis:
      IF (JLOCC.NE.0) THEN
         WRITE (MSGTXT, 1020)
         CALL MSGWRT (8)
         IRET = 3
         GO TO 999
         END IF
C                                       Check number of polarizations:
      IF ((CATBLK(KINAX+JLOCS).NE.2) .AND.
     *   (CATBLK(KINAX+JLOCS).NE.4)) THEN
         WRITE (MSGTXT, 1021)
         CALL MSGWRT (8)
         IRET = 3
         GO TO 999
         END IF
C                                       Check first Stokes parameter
C                                       (must be RR or XX)
      IF ((NINT(CATD(KDCRV+JLOCS)).NE.-1) .AND.
     *   (NINT(CATD(KDCRV+JLOCS)).NE.-5)) THEN
         WRITE (MSGTXT, 1022)
         CALL MSGWRT (8)
         IRET = 3
         GO TO 999
         END IF
C                                       Check Stokes axis increment:
      IF (NINT(CATR(KRCIC+JLOCS)).NE.-1) THEN
         WRITE (MSGTXT, 1022)
         CALL MSGWRT (8)
         IRET = 3
         GO TO 999
         END IF
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SWINIT: ERROR ', I1, ' READING INPUT PARAMETERS')
 1001 FORMAT ('SWINIT: ERROR ', I1, ' INITIALIZING I/O')
 1020 FORMAT ('COMPLEX AXIS MUST BE AXIS ONE --- ABORTING')
 1021 FORMAT ('MUST BE EITHER 2 OR 4 POLARIZATIONS PRESENT --- ',
     *   'ABORTING')
 1022 FORMAT ('POLARIZATIONS INCOMPLETE OR MIXED --- ABORTING')
      END
      SUBROUTINE INITIO (IRET)
C-----------------------------------------------------------------------
C   Open input file, create output file and initialise I/O. Uses file
C   names from SWPFL.INC and sets up I/O buffers in SWPIO.INC.
C
C   Output:
C     IRET      I         Return status: 0 -> OK
C                                        1 -> error opening input file
C                                        2 -> error creating output file
C                                        3 -> error initializing input
C                                        4 -> error initializing output
C-----------------------------------------------------------------------
      INTEGER   IRET
C
C   Local variables:
C     PTYPE      C*2      Physical type of file
C     STAT       C*4      File status
C     IFILE      C*48     Input file physical name
C     OFILE      C*48     Output file physical name
C     BO         I        Block offset to begin I/O transfer from (1)
C     VO         I        Offset of first vis. rec from BO (0)
C     NPIO       I        Number of visibilities to transfer in one I/O
C                         operation (calculated by first call to UVINIT)
C
      INTEGER   BO, VO, NPIO, IERR, I, SCRTCH(256), INCX
      CHARACTER PTYPE*2, STAT*4, IFILE*48, OFILE*48, CTEMP*8
      PARAMETER (BO = 1, VO = 0)
C
      INCLUDE 'SWPFL.INC'
      INCLUDE 'SWPIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Find the input file and read its
C                                       CATBLK:
      INCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', INDISK, INCNO, INNAME, INCLAS, INSEQ, PTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR, INNAME, INCLAS, INSEQ, INDISK,
     *      NLUSER
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
      CALL CATIO ('READ', INDISK, INCNO, CATBLK, 'REST', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
      CALL COPY (256, CATBLK, CATOLD)
C                                       Define pointers into CATBLK:
      CALL UVPGET (IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         GO TO 999
         END IF
C                                       center frequencies
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.LE.0.0) THEN
         UVSCAL = 1.0D0
         DIFPIX = 0.0
      ELSE
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
         END IF
C                                       Make output name and store it in
C                                       CATBLK for the output file:
      CALL MAKOUT (INNAME, INCLAS, INSEQ, ' ', OUTNAM, OUTCLS, OUTSEQ)
      CALL CHR2H (12, OUTNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, OUTCLS, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = OUTSEQ
C                                       Create output file and mark it
C                                       in files common:
      OUTCNO = 1
      FRW(NCFILE) = 3
      CALL UVCREA (OUTDSK, OUTCNO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1002) IERR
         CALL MSGWRT (8)
         IRET = 2
         GO TO 999
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = OUTDSK
      FCNO(NCFILE) = OUTCNO
      FRW(NCFILE) = 2
C                                       copy keywords
      CALL KEYCOP (INDISK, INCNO, OUTDSK, OUTCNO, IERR)
C                                       Mark input file as READ:
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', INDISK, INCNO, INNAME, INCLAS, INSEQ, PTYPE,
     *   NLUSER, 'READ', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1003) IERR
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = INDISK
      FCNO(NCFILE) = INCNO
      FRW(NCFILE) = 0
C                                       Open and initialize input file
C                                       for reading:
      CALL ZPHFIL ('UV', INDISK, INCNO, 1, IFILE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1004) IERR
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
      CALL ZOPEN (INLUN, IFIND, INDISK, IFILE, .TRUE., .FALSE., .FALSE.,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1005) IERR
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
      NPIO = 0
      CALL UVINIT ('READ', INLUN, IFIND, NVIS, VO, LREC, NPIO,
     *   2*UVBFSS, IBUFF, BO, IBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1006) IERR
         CALL MSGWRT (8)
         IRET = 3
         GO TO 999
         END IF
C                                       Open and initialise output file
C                                       for writing:
      CALL ZPHFIL ('UV', OUTDSK, OUTCNO, 1, OFILE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1007) IERR
         CALL MSGWRT (8)
         IRET = 2
         GO TO 999
         END IF
      CALL ZOPEN (OUTLUN, OFIND, OUTDSK, OFILE, .TRUE., .FALSE.,
     *   .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1008) IERR
         CALL MSGWRT (8)
         IRET = 2
         GO TO 999
         END IF
      CALL UVINIT ('WRIT', OUTLUN, OFIND, NVIS, VO, LREC, NPIO,
     *   2*UVBFSS, OBUFF, BO, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1009) IERR
         CALL MSGWRT (8)
         IRET = 4
         GO TO 999
         END IF
C                                       Establish the definitions of
C                                       NIO, VISNO and ILOCWT in
C                                       SWPIO.INC:
      NIO = 0
      VISNO = 0
      ILOCWT = -1
C                                       If data is compressed find the
C                                       offset of the WEIGHT parameter:
      IF (CATBLK(KINAX).EQ.1) THEN
         I = 0
 10      IF ((I.NE.KIPTPN).AND.(ILOCWT.EQ.-1)) THEN
            CALL H2CHR (8, 1, CATH(KHPTP+2*I), CTEMP)
            IF (CTEMP.EQ.'WEIGHT')
     *         ILOCWT = I
            I = I + 1
            GO TO 10
            END IF
         END IF
C                                       If data is compressed then
C                                       adjust the data increments to
C                                       correspond to the expanded data:
      IF (ILOCWT.GE.0) THEN
         INCS = 3 * INCS
         INCF = 3 * INCF
         INCIF = 3 * INCIF
         END IF
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ', I2, ' FINDING ', A12, '.', A6, '.', I4,
     *   ', DISK=', I2, ', USER=', I4)
 1001 FORMAT ('INITIO: ERROR ', I1, ' READING INPUT CATBLK')
 1002 FORMAT ('INITIO: ERROR ', I1, ' CREATING OUTPUT FILE')
 1003 FORMAT ('INITIO: ERROR ', I2, ' CHANGING STATUS OF INPUT FILE')
 1004 FORMAT ('INITIO: ERROR ', I1, ' OBTAINING PHYSICAL NAME OF ',
     *   'INPUT FILE')
 1005 FORMAT ('INITIO: ERROR ', I1, ' OPENING INPUT FILE')
 1006 FORMAT ('INITIO: ERROR ', I1, ' INITIALIZING INPUT FILE')
 1007 FORMAT ('INITIO: ERROR ', I1, ' OBTAINING PHYSICAL NAME OF ',
     *   'OUTPUT FILE')
 1008 FORMAT ('INITIO: ERROR ', I1, ' OPENING OUTPUT FILE')
 1009 FORMAT ('INITIO: ERROR ', I1, ' INITIALIZING OUTPUT FILE')
      END
      SUBROUTINE SWMAIN (IRET)
C-----------------------------------------------------------------------
C   Copy all visibilities from the input to the output file, swapping
C   polarizations when necessary.  The set of polarizations must be
C   (RR, LL), (RR, LL, RL, LR), (XX, YY) or (XX, YY, XY, YX). The
C   fastest varying axis in the visibility data must be the complex
C   axis.
C
C   Output:
C     IRET      I        Return status: I/O error
C-----------------------------------------------------------------------
      INTEGER IRET
C
C   Local variables:
C     NUMCH      I         Number of channels in data
C     NUMIF      I         Number of IFs in data
C     NUMPOL     I         Number of polarizations in data
C     ANT1       I         Antenna 1 in current baseline
C     ANT2       I         Antenna 2 in current baseline
C     SUBA       I         Current subarray
C     CODE       I         Which antennas are swapped on the current
C                          baseline: 0 -> none; 1 -> ant1; 2 -> ant2;
C                          3 -> both
C     POLORG     I(4,0:3)  Maps polarization streams in output to those
C                          in input for each value of CODE.
C     BASE       I         Pointer to current IF-Channel in data
C     VIS        I         Current visibility number
C     IDATA      R(?)      Input data buffer
C     ODATA      R(?)      Output data buffer
C     RPARM      R(16)     Random parameter array
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMCH, NUMIF, NUMPOL, ANT1, ANT2, CODE, POLORG(4, 0:3),
     *   VIS, BASE, IERR, I, ICHAN, IIF, IPOL, SUBA
      REAL      IDATA (3*MAXCIF), ODATA (3*MAXCIF), RPARM(16)
C
      LOGICAL   SWAPAN
      EXTERNAL  SWAPAN
C
      INCLUDE 'SWPAN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C
      DATA POLORG /1, 2, 3, 4,
     *             4, 3, 2, 1,
     *             3, 4, 1, 2,
     *             2, 1, 4, 3/
C-----------------------------------------------------------------------
      NUMIF = 1
      IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
      NUMCH = CATBLK(KINAX+JLOCF)
      NUMPOL = CATBLK(KINAX+JLOCS)
      IF (NUMPOL*NUMCH*NUMIF.GT.MAXCIF) THEN
         WRITE (MSGTXT,1000) NUMPOL, NUMCH, NUMIF, MAXCIF
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
      DO 50 VIS = 1, NVIS
         CALL RDVIS (RPARM, IDATA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1010) VIS, NVIS
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
            END IF
         IF (ILOCB.GE.0) THEN
            ANT1 = NINT (RPARM(1+ILOCB)) / 256
            ANT2 = MOD (NINT (RPARM(1+ILOCB)), 256)
            SUBA = 1.5 + 100.0 * (RPARM(1+ILOCB) -
     *         (256 * ANT1 + ANT2))
         ELSE
            ANT1 = RPARM(1+ILOCA1) + 0.1
            ANT2 = RPARM(1+ILOCA2) + 0.1
            SUBA = RPARM(1+ILOCSA) + 0.1
            END IF
         CODE = 0
         IF (SWAPAN (ANT1))
     *      CODE = CODE + 1
         IF (SWAPAN (ANT2))
     *      CODE = CODE + 2
C                                       Don't swap if incorrect
C                                       subarray:
         IF (SUBA.NE.SUBARR)
     *      CODE = 0
C                                       Copy each IF, channel and
C                                       polarization:
         DO 40 IIF = 1,NUMIF
            DO 30 ICHAN = 1,NUMCH
               BASE = (IIF-1) * INCIF + (ICHAN-1) * INCF + 1
               DO 20 IPOL = 1, NUMPOL
C                                       Loop over complex axis:
                  DO 10 I = 0, 2
                     ODATA(BASE + (IPOL - 1) * INCS + I) =
     *                  IDATA(BASE + (POLORG(IPOL, CODE) - 1)
     *                  * INCS + I)
 10                  CONTINUE
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
         CALL WRVIS (RPARM, ODATA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1011) VIS, NVIS
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
            END IF
 50      CONTINUE
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SWMAIN: NUMBER POLS, CHANS, IFS',I2,I6,I4,
     *   ' EXCEEDS BUFFER',I8)
 1010 FORMAT ('SWMAIN: ERROR READING VISIBILITY ', I6, ' OF ', I6)
 1011 FORMAT ('SWMAIN: ERROR WRITING VISIBILITY ', I6, ' OF ', I6)
      END
      SUBROUTINE RDVIS (RPARM, DATA, IRET)
C-----------------------------------------------------------------------
C   Read one visibility from the input file and return it in
C   uncompressed form. Uses variables in COMMON storage declared in
C   SWPIO.INC and DUVH.INC which must have been initialised by INITIO
C   and UVPGET respectively. There must be at least one unread
C   visibility remaining. RPARM and DATA must be large enough to hold
C   the random parameter array and data array, respectively.
C
C   Outputs:
C     RPARM       R(*)     Random parameter array
C     DATA        R(*)     Visibility data
C     IRET        I        Return status: 0 -> OK
C                                         1 -> error on input file
C
C   Uses, and modifies, variables from SWPIO.INC
C-----------------------------------------------------------------------
      REAL      RPARM(*), DATA(*)
      INTEGER   IRET
C
C   Local variables:
C     VISPTR     I        Pointer to first element in current visibility
C
      INTEGER   IERR, VISPTR
C
      INCLUDE 'SWPIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Read a new block of visibilities
C                                       if the current one is exhausted:
      IF (VISNO.EQ.NIO) THEN
         CALL UVDISK ('READ', INLUN, IFIND, IBUFF, NIO, IBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1000) IERR
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
            END IF
         VISNO = 0
         END IF
C                                       Establish the definition of
C                                       VISPTR:
      VISPTR = IBIND + VISNO * LREC
C                                       Copy random parameters to
C                                       output:
      CALL RCOPY (NRPARM, IBUFF(VISPTR), RPARM)
      RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
      RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
      RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
C                                       Copy data to output,
C                                       decompressing if necessary:
      IF (ILOCWT.GE.0) THEN
         CALL ZUVXPN (LREC-NRPARM, IBUFF(VISPTR+NRPARM),
     *      IBUFF(VISPTR+ILOCWT), DATA)
      ELSE
         CALL RCOPY (LREC-NRPARM, IBUFF(VISPTR+NRPARM), DATA)
         END IF
C                                       Re-establish the definition of
C                                       VISNO:
      VISNO = VISNO + 1
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RDVIS: ERROR ', I1, ' READING INPUT UV DATA')
      END
      SUBROUTINE WRVIS (RPARM, DATA, IRET)
C-----------------------------------------------------------------------
C   Write one visibility to the output file (which must have been read
C   by RDVIS), compressing the data if necessary. The data must not have
C   been modified in any way that changes the length of the random
C   parameter array or the data array. Uses variables from SWPIO.INC and
C   DUVH.INC that must have been initialised with INITIO and UVPGET,
C   respectively.
C
C   Inputs:
C     RPARM      R(*)        Random parameter array
C     DATA       R(*)        Data array
C
C   Output:
C     IRET       I           Return status: 0 -> OK
C                                           1 -> error on output file
C-----------------------------------------------------------------------
      REAL      RPARM(*), DATA(*)
      INTEGER   IRET
C
C   Local variables:
C     VISPTR     I        Pointer to first element of current visibility
C                         in I/O buffer.
C
      INTEGER   IERR, VISPTR
C
      INCLUDE 'SWPIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Establish the definition of
C                                       VISPTR:
      VISPTR = OBIND + (VISNO - 1) * LREC
C                                       Copy random parameters to I/O
C                                       buffer:
      CALL RCOPY (NRPARM, RPARM, OBUFF(VISPTR))
C                                       Copy data to I/O buffer,
C                                       compressing if necessary:
      IF (ILOCWT.GE.0) THEN
         CALL ZUVPAK (LREC-NRPARM, DATA, OBUFF(VISPTR+ILOCWT),
     *      OBUFF(VISPTR+NRPARM))
      ELSE
         CALL RCOPY (LREC-NRPARM, DATA, OBUFF(VISPTR+NRPARM))
         END IF
C                                       If the current record is the
C                                       last in the buffer, flush the
C                                       contents of the buffer to disk:
      IF (VISNO.EQ.NIO) THEN
         CALL UVDISK ('WRIT', OUTLUN, OFIND, OBUFF, NIO, OBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1000) IERR
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
            END IF
         END IF
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WRVIS: ERROR ', I1, ' WRITING TO OUTPUT FILE')
      END
      LOGICAL FUNCTION SWAPAN (ANT)
C-----------------------------------------------------------------------
C   Determine whether polarizations are to be swapped for antenna ANT.
C
C   Input:
C     ANT     I      Antenna number
C
C   Output:
C     SWAPAN  L      .TRUE. if ANT is to be swapped, otherwise .FALSE.
C
C   Uses variables in local include SWPAN.INC
C-----------------------------------------------------------------------
      INTEGER   ANT
C
      LOGICAL   FOUND, RESULT
      INTEGER   I
C
      INCLUDE 'SWPAN.INC'
C-----------------------------------------------------------------------
C                                       Check whether all antennae
C                                       wanted:
      IF (NANT.EQ.0) THEN
         RESULT = .TRUE.
      ELSE
C                                       Check if ANT is in antenna list:
         I = 0
         FOUND = .FALSE.
 10      IF ((I.NE.NANT) .AND. (.NOT.FOUND)) THEN
            I = I + 1
            IF (ANT.EQ.ANTENS(I))
     *         FOUND = .TRUE.
            GO TO 10
            END IF
C                                       Check whether antennas in list
C                                       are selected or deselected:
         IF (INCLUD) THEN
            RESULT = FOUND
         ELSE
            RESULT = .NOT. FOUND
            END IF
         END IF
      SWAPAN = RESULT
      RETURN
      END
      SUBROUTINE ENDUV (DOCAL, IRET)
C-----------------------------------------------------------------------
C   Flush the output buffer and close both the input and output UV
C   files, copy tables from the input file to the output file and swap
C   calibration information in all CL and SN tables if the user asked
C   for this. Uses variables from SWPIO.INC which must have been
C   initialised with INITIO and from SWPFL.INC which must have been
C   initialised with SWPIN.
C
C   NOTE: does not need to compress output file since number of
C   visibilities is not changed.
C
C   Input:
C     DOCAL      L        .TRUE. if polarization calibration information
C                         in SN and CL tables should be swapped; .FALSE.
C                         otherwise
C
C   Output:
C     IRET       I        Return status: 0 -> OK
C                                        1 -> error on output file
C                                        2 -> error closing file
C                                        3 -> error copying tables
C                                        4 -> error updating cal.
C-----------------------------------------------------------------------
      LOGICAL   DOCAL(2)
      INTEGER   IRET
C
C   Local variables:
C     TILUN       I        Input LUN for tables (constant)
C     TOLUN       I        Output LUN for tables (constant)
C     TIBUF       I(256)   Input buffer for tables I/O
C     TOBUF       I(256)   Output buffer for tables I/O
C
      INTEGER   TILUN, TOLUN
      PARAMETER (TILUN = 27, TOLUN = 28)
      INTEGER   TIBUF(512), TOBUF(512)
      INTEGER   IERR, NONOT
      CHARACTER NOTTYP(3)*2
C
      INCLUDE 'SWPFL.INC'
      INCLUDE 'SWPIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NONOT, NOTTYP /3, 'CD','SY','TY'/
C-----------------------------------------------------------------------
C                                       Flush output buffer:
      NIO = 0
      CALL UVDISK ('FLSH', OUTLUN, OFIND, OBUFF, NIO, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
C                                       Close files:
      CALL ZCLOSE (INLUN, IFIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (8)
         IRET = 2
         GO TO 999
         END IF
      CALL ZCLOSE (OUTLUN, OFIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1002) IERR
         CALL MSGWRT (8)
         IRET = 2
         GO TO 999
         END IF
C                                       Copy tables:
      IF (.NOT.DOCAL(2)) NONOT = 0
      CALL ALLTAB (NONOT, NOTTYP, TILUN, TOLUN, INDISK, OUTDSK, INCNO,
     *   OUTCNO, CATBLK, TIBUF, TOBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1003) IERR
         CALL MSGWRT (8)
         IRET = 3
         GO TO 999
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (OUTDSK, OUTCNO, DIFPIX, TIBUF, TOBUF, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'ENDUV: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                       Swap polarization calibration,
C                                       if requested:
      IF (DOCAL(1)) THEN
         CALL SWPCAL (IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1004) IERR
            CALL MSGWRT (8)
            IRET = 4
            GO TO 999
            END IF
         END IF
      IF (DOCAL(2)) THEN
         CALL FIXTAB (TILUN, TOLUN, INDISK, OUTDSK, INCNO, OUTCNO,
     *      CATOLD, CATBLK, TIBUF, TOBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1210) IERR
            CALL MSGWRT (7)
            END IF
         END IF
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ENDUV: ERROR ', I1, ' WRITING TO OUTPUT FILE')
 1001 FORMAT ('ENDUV: ERROR ', I1, ' CLOSING INPUT FILE')
 1002 FORMAT ('ENDUV: ERROR ', I1, ' CLOSING OUTPUT FILE')
 1003 FORMAT ('ENDUV: ERROR ', I1, ' COPYING TABLES')
 1004 FORMAT ('ENDUV: ERROR ', I1, ' UPDATING CALIBRATION')
 1210 FORMAT ('ENDUV: ERROR',I3,' FIXING SY, TY, CD TABLES')
      END
      SUBROUTINE SWPHIS (DOCAL, IRET)
C-----------------------------------------------------------------------
C   Copy and update history file.
C
C   Input:
C     DOCAL      L       .TRUE. if calibration was swapped.
C
C   Output:
C     IRET       I       Return status: 0 -> OK
C                                       1 -> I/O error
C-----------------------------------------------------------------------
      LOGICAL   DOCAL
      INTEGER   IRET
C
C   Local variables:
C     HILUN      I      LUN for input history file
C     HOLUN      I      LUN for output history file
C     HILINE     C*72   History record buffer
C     BUFER1     I(256) Extension file I/O buffer
C     BUFER2     I(256) Extension file I/O buffer
C
      INTEGER   HILUN, HOLUN, BUFER1(256), BUFER2(256), LIMIT, LIMIT2,
     *   I, IERR
      CHARACTER HILINE*72
C
      PARAMETER (HILUN = 27, HOLUN = 28)
C
      INCLUDE 'SWPAN.INC'
      INCLUDE 'SWPFL.INC'
      INCLUDE 'SWPIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL HIINIT (3)
C                                       Copy history file:
      CALL HISCOP (HILUN, HOLUN, INDISK, OUTDSK, INCNO, OUTCNO,
     *   CATBLK, BUFER1, BUFER2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Add input file description:
      CALL HENCO1 (TSKNAM, INNAME, INCLAS, INSEQ, INDISK, HOLUN,
     *   BUFER2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Add output file description:
      CALL HENCOO (TSKNAM, OUTNAM, OUTCLS, OUTSEQ, OUTDSK, HOLUN,
     *   BUFER2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Add antennas adverb:
      IF (NANT.EQ.0) THEN
         WRITE (HILINE, 1002)
      ELSE IF (INCLUD) THEN
         WRITE (HILINE, 1003)
         CALL HIADD (HOLUN, HILINE, BUFER2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1001) IERR
            CALL MSGWRT (6)
            GO TO 100
            END IF
      ELSE
         WRITE (HILINE, 1004)
         CALL HIADD (HOLUN, HILINE, BUFER2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1001) IERR
            CALL MSGWRT (6)
            GO TO 100
            END IF
         END IF
      LIMIT2 = MIN (12, NANT)
      WRITE (HILINE, 1005) TSKNAM, (ANTENS(I), I = 1, LIMIT2)
      CALL HIADD (HOLUN, HILINE, BUFER2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
      IF (LIMIT2.LT.NANT) THEN
         DO 10 LIMIT = 13, NANT, 12
            LIMIT2 = MIN (LIMIT+11, NANT)
            WRITE (HILINE, 1006) TSKNAM, (ANTENS(I), I=LIMIT,LIMIT2)
            CALL HIADD (HOLUN, HILINE, BUFER2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1001) IERR
               CALL MSGWRT (6)
               GO TO 100
               END IF
 10         CONTINUE
         END IF
C                                       Subarray:
      WRITE (HILINE, 1007) TSKNAM, SUBARR
      CALL HIADD (HOLUN, HILINE, BUFER2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Was calibration swapped:
      IF (DOCAL) THEN
         WRITE (HILINE, 1008) TSKNAM
      ELSE
         WRITE (HILINE, 1009) TSKNAM
         END IF
      CALL HIADD (HOLUN, HILINE, BUFER2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Close history file:
 100  CALL HICLOS (HOLUN, .TRUE., BUFER2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1100) IERR
         CALL MSGWRT (6)
         END IF
C                                       Update output file header:
      CALL CATIO ('UPDT', OUTDSK, OUTCNO, CATBLK, 'REST', BUFER1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1101) IERR
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SWPHIS: ERROR ', I1, ' COPYING HISTORY FILE')
 1001 FORMAT ('SWPHIS: ERROR ', I1, ' WRITING TO HISTORY FILE')
 1002 FORMAT (A6, ' ANTENNAS = 0 / All antennas swapped')
 1003 FORMAT (A6, ' / Polarizations swapped for listed antennas')
 1004 FORMAT (A6, ' / Polarizations swapped for all but listed ',
     *   'antennas')
 1005 FORMAT (A6, ' ANTENNAS = ', 12(I3, ' '))
 1006 FORMAT (A6, '            ', 12(I3, ' '))
 1007 FORMAT (A6, ' SUBARRAY = ', I2)
 1008 FORMAT (A6, ' DOCALIB  = TRUE / CL and SN tables updated')
 1009 FORMAT (A6, ' DOCALIB  = FALSE / CL and SN tables left unchanged')
 1100 FORMAT ('SWPHIS: ERROR ', I1, ' CLOSING HISTORY FILE')
 1101 FORMAT ('SWPHIS: ERROR ', I1, ' UPDATING CATBLK')
      END
      SUBROUTINE SWPCAL (IRET)
C-----------------------------------------------------------------------
C   Swap calibration antenna for each polarization for selected
C   antennae.
C
C   Outputs:
C     IRET           I       Return code: 0 -> OK
C                                         1 -> I/O error
C                                         2 -> bad table
C-----------------------------------------------------------------------
      INTEGER   IRET
C
C   Local variables:
C     TABLUN      I         LUN to use for tables (constant)
C     NUMCL       I         Number of CL tables present
C     NUMSN       I         Number of SN tables present
C     TABUFF      I(512)    Tables I/O buffer
C     NUMANT      I         Number of antennae in current table (unused)
C     NUMPOL      I         Number of polarizations in current table
C                           (should be 2)
C     NUMIF       I         Number of IFs in current table
C     NUMNOD      I         Number of interpolation nodes in SN table
C     RANOD       R(32)     RA offset of each node
C     DECNOD      R(32)     Declination offset of each node
C     GMMOD       R         Mean gain modulus (unused)
C     ROWNUM      I         Current row number in current table
C     LSTROW      I         Last row in current table
C     TAKOLS      I(MAXCLC) Column pointer array
C     TANUMV      I(MAXCLC) Element count for each column
C  Most others hold calibration information from CL or SN table: see
C  headers for TABCAL and TABSN for descriptions.
C
      INTEGER   TABLUN
      PARAMETER (TABLUN = 27)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMCL, NUMSN, TABUFF(1024), NUMANT, NUMPOL, NUMIF,
     *   NUMNOD, ROWNUM, LSTROW, TAKOLS(MAXCLC),
     *   TANUMV(MAXCLC), I, J, IF, IERR, ITEMP
      REAL      RANOD(32), DECNOD(32), GMMOD, RTEMP
C
      INTEGER   SOURID, ANTNO, SUBA, FREQID, REFA(2, MAXIF),
     *   NODENO, NTERM
      REAL      DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF)
      DOUBLE PRECISION    GEODLY(8)
      REAL      TIMEI, IFR
      DOUBLE PRECISION TIME
      LOGICAL   ISAPPL
C
      LOGICAL   SWAPAN
      EXTERNAL  SWAPAN
C
      INCLUDE 'SWPAN.INC'
      INCLUDE 'SWPFL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Set number of polarizations and
C                                       IFs:
      NUMPOL = 2
      NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Establish the definitions of
C                                       NUMCL and NUMSN
      CALL FNDEXT ('CL', CATBLK, NUMCL)
      CALL FNDEXT ('SN', CATBLK, NUMSN)
C                                       Process all CL files:
      DO 120 I = 1, NUMCL
         CALL CALINI ('WRIT', TABUFF, OUTDSK, OUTCNO, I, CATBLK, TABLUN,
     *      J, TAKOLS, TANUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1010) IERR, I
            CALL MSGWRT (7)
            GO TO 120
            END IF
C                                       Process each record:
         LSTROW = J - 1
         DO 110 J = 1, LSTROW
            ROWNUM = J
            CALL TABCAL ('READ', TABUFF, ROWNUM, TAKOLS, TANUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *         GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *         DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *         IERR)
C                                       Deselected record.
            IF (IERR.LT.0) GO TO 110
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1011) IERR, I
               CALL MSGWRT (8)
               IRET = 1
               GO TO 999
               END IF
            IF (SWAPAN(ANTNO) .AND. (SUBA.EQ.SUBARR)) THEN
               RTEMP = MBDELY(1)
               MBDELY(1) = MBDELY(2)
               MBDELY(2) = RTEMP
               RTEMP = CLOCK(1)
               CLOCK(1) = CLOCK(2)
               CLOCK(2) = RTEMP
               RTEMP = DCLOCK(1)
               DCLOCK(1) = DCLOCK(2)
               DCLOCK(2) = RTEMP
               RTEMP = DISP(1)
               DISP(1) = DISP(2)
               DISP(2) = RTEMP
               RTEMP = DDISP(1)
               DDISP(1) = DDISP(2)
               DDISP(2) = RTEMP
               DO 100 IF = 1, NUMIF
                  RTEMP = CREAL(1, IF)
                  CREAL(1, IF) = CREAL(2, IF)
                  CREAL(2, IF) = RTEMP
                  RTEMP = CIMAG(1, IF)
                  CIMAG(1, IF) = CIMAG(2, IF)
                  CIMAG(2, IF) = RTEMP
                  RTEMP = DELAY(1, IF)
                  DELAY(1, IF) = DELAY(2, IF)
                  DELAY(2, IF) = RTEMP
                  RTEMP = RATE(1, IF)
                  RATE(1, IF) = RATE(2, IF)
                  RATE(2, IF) = RTEMP
                  RTEMP = WEIGHT(1, IF)
                  WEIGHT(1, IF) = WEIGHT(2, IF)
                  WEIGHT(2, IF) = RTEMP
                  ITEMP = REFA(1, IF)
                  REFA(1, IF) = REFA(2, IF)
                  REFA(2, IF) = ITEMP
 100              CONTINUE
               END IF
            ROWNUM = J
            CALL TABCAL ('WRIT', TABUFF, ROWNUM, TAKOLS, TANUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *         GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *         DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1100) IERR, I
               CALL MSGWRT (8)
               IRET = 1
               GO TO 999
               END IF
 110        CONTINUE
         IERR = 0
         CALL TABCAL ('CLOS', TABUFF, ROWNUM, TAKOLS, TANUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *      DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1110) IERR, I
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
            END IF
         WRITE (MSGTXT, 1111) I
         CALL MSGWRT (3)
 120     CONTINUE
C                                       Process each SN table:
      DO 220 I = 1, NUMSN
         CALL SNINI ('WRIT', TABUFF, OUTDSK, OUTCNO, I, CATBLK, TABLUN,
     *      J, TAKOLS, TANUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *      RANOD, DECNOD, ISAPPL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1120) IERR, I
            CALL MSGWRT (7)
            GO TO 220
            END IF
C                                       Process each row
         LSTROW = J - 1
         DO 210 J = 1, LSTROW
            ROWNUM = J
            CALL TABSN ('READ', TABUFF, ROWNUM, TAKOLS, TANUMV, NUMPOL,
     *         TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *         MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *         REFA, IERR)
C                                       Deselected record.
            IF (IERR.LT.0) GO TO 210
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1121) IERR, I
               CALL MSGWRT (8)
               IRET = 1
               GO TO 999
               END IF
C                                       Swap info this antenna?
            IF (SWAPAN(ANTNO) .AND. (SUBA.EQ.SUBARR)) THEN
C                                       Multiband delays
               RTEMP = MBDELY(1)
               MBDELY(1) = MBDELY(2)
               MBDELY(2) = RTEMP
               RTEMP = DISP(1)
               DISP(1) = DISP(2)
               DISP(2) = RTEMP
               RTEMP = DDISP(1)
               DDISP(1) = DDISP(2)
               DDISP(2) = RTEMP
C                                       Loop over IFs:
               DO 200 IF = 1, NUMIF
                  RTEMP = CREAL(1, IF)
                  CREAL(1, IF) = CREAL(2, IF)
                  CREAL(2, IF) = RTEMP
                  RTEMP = CIMAG(1, IF)
                  CIMAG(1, IF) = CIMAG(2, IF)
                  CIMAG(2, IF) = RTEMP
                  RTEMP = DELAY(1, IF)
                  DELAY(1, IF) = DELAY(2, IF)
                  DELAY(2, IF) = RTEMP
                  RTEMP = RATE(1, IF)
                  RATE(1, IF) = RATE(2, IF)
                  RATE(2, IF) = RTEMP
                  RTEMP = WEIGHT(1, IF)
                  WEIGHT(1, IF) = WEIGHT(2, IF)
                  WEIGHT(2, IF) = RTEMP
                  ITEMP = REFA(1, IF)
                  REFA(1, IF) = REFA(2, IF)
                  REFA(2, IF) = ITEMP
 200              CONTINUE
               END IF
            ROWNUM = J
            CALL TABSN ('WRIT', TABUFF, ROWNUM, TAKOLS, TANUMV, NUMPOL,
     *         TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *         MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *         REFA, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1200) IERR, I
               CALL MSGWRT (8)
               IRET = 1
               GO TO 999
               END IF
 210        CONTINUE
         IERR = 0
         CALL TABSN ('CLOS', TABUFF, ROWNUM, TAKOLS, TANUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1210) IERR, I
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
            END IF
         WRITE (MSGTXT, 1211) I
         CALL MSGWRT (3)
 220     CONTINUE
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SWPCAL: ERROR ', I1, ' OPENING CL TABLE VERSION ', I2)
 1011 FORMAT ('SWPCAL: ERROR ', I1, ' READING CL TABLE VERSION ', I2)
 1100 FORMAT ('SWPCAL: ERROR ', I1, ' WRITING CL TABLE VERSION ', I2)
 1110 FORMAT ('SWPCAL: ERROR ', I1, ' CLOSING CL TABLE VERSION ', I2)
 1111 FORMAT ('Corrected CL table version ', I2)
 1120 FORMAT ('SWPCAL: ERROR ', I1, ' OPENING SN TABLE VERSION ', I2)
 1121 FORMAT ('SWPCAL: ERROR ', I1, ' READING SN TABLE VERSION ', I2)
 1200 FORMAT ('SWPCAL: ERROR ', I1, ' WRITING SN TABLE VERSION ', I2)
 1210 FORMAT ('SWPCAL: ERROR ', I1, ' CLOSING SN TABLE VERSION ', I2)
 1211 FORMAT ('Corrected SN table version ', I2)
      END
      SUBROUTINE FIXTAB (LUNI, LUNO, DISKI, DISKO, CNOI, CNOO, CATOLD,
     *   CATBLK, BUFF1, BUFF2, IERR)
C-----------------------------------------------------------------------
C   swap SY, TY, CD tables, copying to output
C   Inputs:
C      NUMANT   I      Number in ANTS
C      ANTS     I(*)   Antennas to swap
C      LUNI     I      Input LUN
C      LUNO     I      Output LUN
C      DISKI    I      Input disk
C      DISKO    I      Output disk
C      CNOI     I      Input catalog number
C      CNOO     I      Input catalog number
C      CATOLD   I(*)   Input header
C   In/Out
C      CATBLK   I(*)   Output header
C      BUFF1    I(*)   buffer 1
C      BUFF2    I(*)   buffer 2
C   Output:
C      IERR     I      error code
C-----------------------------------------------------------------------
      INTEGER   LUNI, LUNO, DISKI, DISKO, CNOI, CNOO, CATOLD(256),
     *   CATBLK(256), BUFF1(*), BUFF2(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NVER, NREC, IVER, RNO, KOLS(20), NUMV(20), NUMA, I,
     *   NUMPOL, NUMIF, IREC, ANTNO, SUBA, FREQID, SOURID
      CHARACTER RDATE*8
      REAL      TIMEI, TCAL(2,MAXIF), PDIFF(2,MAXIF), PSUM(2,MAXIF),
     *   PGAIN(2,MAXIF), TSYS(2,MAXIF), TANT(2,MAXIF), TEMP
      DOUBLE PRECISION TIME
      LOGICAL   SWAP, SWAPAN
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       CD tables
      CALL FNDEXT ('CD', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         DO 100 IVER = 1,NVER
            CALL CDINI ('READ', BUFF1, DISKI, CNOI, IVER, CATOLD, LUNI,
     *         RNO, KOLS, NUMV, NUMA, NUMPOL, NUMIF, RDATE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN INPUT CD', IVER
               CALL MSGWRT (7)
               GO TO 100
               END IF
            NREC = BUFF1(5)
            CALL CDINI ('WRIT', BUFF2, DISKO, CNOO, IVER, CATBLK, LUNO,
     *         RNO, KOLS, NUMV, NUMA, NUMPOL, NUMIF, RDATE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT CD', IVER
               CALL MSGWRT (7)
               GO TO 90
               END IF
            DO 50 IREC = 1,NREC
               RNO = IREC
               CALL TABCD ('READ', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, ANTNO, SUBA, FREQID, TCAL, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ INPUT CD', IVER
                  CALL MSGWRT (7)
                  GO TO 90
                  END IF
               SWAP = SWAPAN (ANTNO)
               IF (SWAP) THEN
                  DO 30 I = 1,NUMIF
                     TEMP = TCAL(1,I)
                     TCAL(1,I) = TCAL(2,I)
                     TCAL(2,I) = TEMP
 30                  CONTINUE
                  END IF
               RNO = IREC
               CALL TABCD ('WRIT', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, ANTNO, SUBA, FREQID, TCAL, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ OUTPUT CD', IVER
                  CALL MSGWRT (7)
                  GO TO 90
                  END IF
 50            CONTINUE
            WRITE (MSGTXT,1050) 'CD', DISKI, CNOI, IVER, DISKO, CNOO,
     *         IVER
            CALL MSGWRT (3)
C                                       close
 90         RNO = NREC
            CALL TABCD ('CLOS', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, ANTNO, SUBA, FREQID, TCAL, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE OUTPUT CD', IVER
               CALL MSGWRT (7)
               END IF
            RNO = NREC
            CALL TABCD ('CLOS', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, ANTNO, SUBA, FREQID, TCAL, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE INPUT CD', IVER
               CALL MSGWRT (7)
               END IF
 100        CONTINUE
         END IF
C                                       SY tables
      CALL FNDEXT ('SY', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         DO 200 IVER = 1,NVER
            CALL SYINI ('READ', BUFF1, DISKI, CNOI, IVER, CATOLD, LUNI,
     *         RNO, KOLS, NUMV, NUMA, NUMPOL, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN INPUT SY', IVER
               CALL MSGWRT (7)
               GO TO 200
               END IF
            NREC = BUFF1(5)
            CALL SYINI ('WRIT', BUFF2, DISKO, CNOO, IVER, CATBLK, LUNO,
     *         RNO, KOLS, NUMV, NUMA, NUMPOL, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT SY', IVER
               CALL MSGWRT (7)
               GO TO 190
               END IF
            DO 150 IREC = 1,NREC
               RNO = IREC
               CALL TABSY ('READ', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            PDIFF, PSUM, PGAIN, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ INPUT SY', IVER
                  CALL MSGWRT (7)
                  GO TO 190
                  END IF
               SWAP = SWAPAN (ANTNO)
               IF (SWAP) THEN
                  DO 130 I = 1,NUMIF
                     TEMP = PDIFF(1,I)
                     PDIFF(1,I) = PDIFF(2,I)
                     PDIFF(2,I) = TEMP
                     TEMP = PSUM(1,I)
                     PSUM(1,I) = PSUM(2,I)
                     PSUM(2,I) = TEMP
                     TEMP = PGAIN(1,I)
                     PGAIN(1,I) = PGAIN(2,I)
                     PGAIN(2,I) = TEMP
 130                 CONTINUE
                  END IF
               RNO = IREC
               CALL TABSY ('WRIT', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            PDIFF, PSUM, PGAIN, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ OUTPUT SY', IVER
                  CALL MSGWRT (7)
                  GO TO 190
                  END IF
 150           CONTINUE
            WRITE (MSGTXT,1050) 'SY', DISKI, CNOI, IVER, DISKO, CNOO,
     *         IVER
            CALL MSGWRT (3)
C                                       close
 190        RNO = NREC
            CALL TABSY ('CLOS', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *         PDIFF, PSUM, PGAIN, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE OUTPUT SY', IVER
               CALL MSGWRT (7)
               END IF
            RNO = NREC
            CALL TABSY ('CLOS', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *         PDIFF, PSUM, PGAIN, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE INPUT SY', IVER
               CALL MSGWRT (7)
               END IF
 200        CONTINUE
         END IF
C                                       TY tables
      CALL FNDEXT ('TY', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         DO 300 IVER = 1,NVER
            CALL TYINI ('READ', BUFF1, DISKI, CNOI, IVER, CATOLD, LUNI,
     *         RNO, KOLS, NUMV, NUMPOL, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN INPUT TY', IVER
               CALL MSGWRT (7)
               GO TO 300
               END IF
            NREC = BUFF1(5)
            CALL TYINI ('WRIT', BUFF2, DISKO, CNOO, IVER, CATBLK, LUNO,
     *         RNO, KOLS, NUMV, NUMPOL, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT TY', IVER
               CALL MSGWRT (7)
               GO TO 290
               END IF
            DO 250 IREC = 1,NREC
               RNO = IREC
               CALL TABTY ('READ', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            TSYS, TANT, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ INPUT TY', IVER
                  CALL MSGWRT (7)
                  GO TO 290
                  END IF
               SWAP = SWAPAN (ANTNO)
               IF (SWAP) THEN
                  DO 230 I = 1,NUMIF
                     TEMP = TSYS(1,I)
                     TSYS(1,I) = TSYS(2,I)
                     TSYS(2,I) = TEMP
                     TEMP = TANT(1,I)
                     TANT(1,I) = TANT(2,I)
                     TANT(2,I) = TEMP
 230                 CONTINUE
                  END IF
               RNO = IREC
               CALL TABTY ('WRIT', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            TSYS, TANT, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ OUTPUT TY', IVER
                  CALL MSGWRT (7)
                  GO TO 290
                  END IF
 250           CONTINUE
            WRITE (MSGTXT,1050) 'TY', DISKI, CNOI, IVER, DISKO, CNOO,
     *         IVER
            CALL MSGWRT (3)
C                                       close
 290        RNO = NREC
            CALL TABTY ('CLOS', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *         TSYS, TANT, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE OUTPUT TY', IVER
               CALL MSGWRT (7)
               END IF
            RNO = NREC
            CALL TABTY ('CLOS', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *         TSYS, TANT, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE INPUT TY', IVER
               CALL MSGWRT (7)
               END IF
 300        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIXTAB ERROR:',I4,' ON ',A,' TABLE VERS',I4)
 1050 FORMAT ('Swapped ',A2,' file from vol/cno/vers',I3,I5,I4,' to',
     *   I3,I5,I4)
      END
