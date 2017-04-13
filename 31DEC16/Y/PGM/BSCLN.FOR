LOCAL INCLUDE 'BSCLN.INC'
      INCLUDE 'INCS:PMAD.INC'
C
      HOLLERITH XNAME1(3), XCLAS1(2), XNAME2(3), XCLAS2(2), XNAMEO(3),
     *   XCLASO(2)
      REAL      XSEQ1, XDISK1, XSEQ2, XDISK2, XSEQO, XDISKO, BLC(7),
     *   TRC(7), DPARM(10), XNIT, XFLUX, GAIN, BMAJ, XNBOX, XBOX(4,50),
     *   DOTV
      COMMON /INPARM/ XNAME1, XCLAS1, XSEQ1, XDISK1, XNAME2, XCLAS2,
     *   XSEQ2, XDISK2, XNAMEO, XCLASO, XSEQO, XDISKO, BLC, TRC, DPARM,
     *   XNIT, XFLUX, GAIN, BMAJ, XNBOX, XBOX, DOTV
C
      CHARACTER NAME1*12, CLAS1*6, NAME2*12, CLAS2*6, NAMOUT*12,
     *   CLASO*6, CUNITS*8
      INTEGER   SEQ1, SEQ2, SEQO, DISK1, DISK2, DISKO, SLOT1, SLOT2,
     *   SLOTO, LUN1, LUN2, FIND1, FIND2, LUNO, FINDO, IWIN(4), JBUFSZ,
     *   SCRTCH(256), CATOLD(256), XOFF, YOFF, NITER, NBOXES,
     *   CLBOX(4,50)
      LOGICAL   WASBLK
      REAL      BUFF1(MABFSS), BUFF2(MABFSS), BUFFO(MABFSS), RMIN, RMAX
      COMMON /BSCHAR/ NAME1, CLAS1, NAME2, CLAS2, NAMOUT, CLASO, CUNITS
      COMMON /BSPARM/ CATOLD, BUFF1, BUFF2, BUFFO, SCRTCH, SEQ1, SEQ2,
     *   SEQO, DISK1, DISK2, DISKO, SLOT1, SLOT2, SLOTO, LUN1, LUN2,
     *   FIND1, FIND2, LUNO, FINDO, IWIN, JBUFSZ, RMIN, RMAX, XOFF,
     *   YOFF, WASBLK, NITER, NBOXES, CLBOX
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
LOCAL END
      PROGRAM BSCLN
C-----------------------------------------------------------------------
C! Hogbom-like Clean on beam-switched difference image
C# Singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1999, 2002, 2005, 2008, 2012
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
C   BSCLN will do a hogbom-like Clean on a beam switched image using
C   Gaussians as the beam model.
C   INPUTS:  (from AIPS)
C      INNAME   R(3)   the name of the input file
C      INCLASS  R(2)   the class of the input file
C      INSEQ    R      the sequence number of the input file.
C      INDISK   R      the disk volume number of the input file.
C      OUTNAME  R(3)   the name of the Cleaned file.
C      OUTCLASS R(2)   the class of the Cleaned file.
C      OUTSEQ   R      the sequence number for the Cleaned file.
C      OUTDISK  R      the disk volume number for the Cleaned
C      BLC      R(7)   Bottom left corner of input to be Cleaned in
C                      pixels.
C      TRC      R(7)   Top right corner in pixels to be Cleaned
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
C
      INTEGER   NX, NY, NB, NWORD, IRET
      LONGINT   IPR, IPC
      REAL      RESID(2), COMP(2), BEAM(5000)
      INCLUDE 'BSCLN.INC'
      DATA PRGNAM /'BSCLN '/
C-----------------------------------------------------------------------
C                                       Initialize the task
      CALL BSCINI (PRGNAM, NX, NY, NB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read in image, compute beams
      NWORD = (NX * NY - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'BSCLN', NWORD, RESID, IPR, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL ZMEMRY ('GET ', 'BSCLN', NWORD, COMP, IPC, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL BSCSET (NB, BEAM, NX, NY, RESID(1+IPR), COMP(1+IPC), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Clean the thing
      CALL BSCLEN (NB, BEAM, NX, NY, RESID(1+IPR), COMP(1+IPC), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Output the thing
      CALL BSCOUT (NB, BEAM, NX, NY, RESID(1+IPR), COMP(1+IPC), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History file, et al.
      CALL BSCHIS
C                                       Quit
C                                       Release AIPS if wait state.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE BSCINI (PRGNAM, NX, NY, NB, IRET)
C-----------------------------------------------------------------------
C   Inits BSCLN determining image sizes, beam sizes, etc
C   Inputs:
C      PRGNAM   C*6   Program name
C   Output:
C      NX       I     Image X pixel dimension
C      NY       I     Image Y pixel dimension
C      NB       I     Beam pixel dimension
C      IRET     I     Error code
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   NX, NY, NB, IRET
C
      INTEGER   INPRMS, IUSER, INODIM, NUMKEY, KEYTYP, LOCS, I, IERR,
     *   IROUND
      REAL      TEMP
      CHARACTER MTYPE*2, BLANK*6
      INCLUDE 'BSCLN.INC'
      DATA BLANK /' '/
C-----------------------------------------------------------------------
C                                       Initialize the I/O parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      CALL FILL (10, 0, IBAD)
C                                       Get input values from AIPS.
      INPRMS = 5 * 7 + 10 + 6 + 200
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XNAME1, SCRTCH, IERR)
      IRET = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         END IF
C
      IF (RQUICK) CALL RELPOP (IRET, CATBLK, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAME1, NAME1)
      CALL H2CHR (6, 1, XCLAS1, CLAS1)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMEO, NAMOUT)
      CALL H2CHR (6, 1, XCLASO, CLASO)
C                                       Set initial values.
      DISK1 = IROUND (XDISK1)
      SEQ1 = IROUND (XSEQ1)
      DISK2 = IROUND (XDISK2)
      SEQ2 = IROUND (XSEQ2)
      DISKO = IROUND (XDISKO)
      SEQO = IROUND (XSEQO)
      IUSER = NLUSER
      NITER = XNIT + 0.1
      IF (NITER.LE.0) NITER = 100
      IF (GAIN.LE.0.0) GAIN = 0.1
      IF (GAIN.GT.0.99) GAIN = 0.1
C                                       Open source file.
      LUN1 = 18
      LUN2 = 19
      LUNO = 20
      MTYPE = 'MA'
      CALL MAPOPN ('READ', DISK2, NAME2, CLAS2, SEQ2, MTYPE, IUSER,
     *   LUN2, FIND2, SLOT2, CATBLK, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 999
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK2
      FCNO(NCFILE) = SLOT2
      FRW(NCFILE) = 0
      CALL MAPOPN ('READ', DISK1, NAME1, CLAS1, SEQ1, MTYPE, IUSER,
     *   LUN1, FIND1, SLOT1, CATBLK, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 999
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK1
      FCNO(NCFILE) = SLOT1
      FRW(NCFILE) = 0
      CALL H2CHR (8, 1, CATH(KHBUN), CUNITS)
C                                       Set default values BLC, TRC.
      INODIM = CATBLK(KIDIM)
      CALL WINDOW (INODIM, CATBLK(KINAX), BLC, TRC, IRET)
      IF (IRET.NE.0) GO TO 999
      IWIN(1) = IROUND (BLC(1))
      IWIN(2) = IROUND (BLC(2))
      IWIN(3) = IROUND (TRC(1))
      IWIN(4) = IROUND (TRC(2))
      NX = IWIN(3) - IWIN(1) + 1
      NY = IWIN(4) - IWIN(2) + 1
C                                       Build new file cat name.
      CALL MAKOUT (NAME1, CLAS1, SEQ1, BLANK, NAMOUT, CLASO, SEQO)
C                                       Set header values needed
C                                       MCREAT.
      CALL COPY (256, CATBLK, CATOLD)
      CATBLK(KINAX) = NX
      CATBLK(KINAX+1) = NY
      CATBLK(KRCRP) = CATBLK(KRCRP) - BLC(1) + 1.0
      CATBLK(KRCRP+1) = CATBLK(KRCRP+1) - BLC(2) + 1.0
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASO, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQO
C                                       Create new cataloged file.
      CALL MCREAT (DISKO, SLOTO, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 999
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = SLOTO
      FRW(NCFILE) = 2
      SEQO = CATBLK(KIIMS)
C                                       copy some keywords
      CALL KEYPCP (DISK1, SLOT1, DISKO, SLOTO, 0, ' ', IERR)
C                                       Open new file.
      MTYPE = 'MA'
      CALL MAPOPN ('INIT', DISKO, NAMOUT, CLASO, SEQO, MTYPE, IUSER,
     *   LUNO, FINDO, SLOTO, CATBLK, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Make beam parameters
      IF (DPARM(3).LT.0.1) DPARM(3) = 1.0
      IF (DPARM(4).LT.0.1) DPARM(4) = 1.0
      IF (DPARM(5).LT.0.1) DPARM(5) = 1.0
      NUMKEY = 1
      CALL CATKEY ('READ', DISK1, SLOT1, 'BSTHROW ', NUMKEY, LOCS,
     *   TEMP, KEYTYP, SCRTCH, IRET)
      IF (IRET.NE.0) TEMP = 0.5
      TEMP = TEMP * 2.0
      IF (DPARM(1).EQ.0.0) DPARM(1) = 1.0
      DPARM(1) = TEMP * DPARM(1)
      IF (CATR(KRCIC).EQ.0.0) CATR(KRCIC) = 1.0
      IF (CATR(KRBMJ).GT.0.0) THEN
         TEMP = CATR(KRBMJ) * 3600.
      ELSE
         TEMP = 3.5 * 3600. * ABS(CATR(KRCIC))
         END IF
      IF (BMAJ.EQ.0.0) BMAJ = TEMP
      DPARM(4) = DPARM(4) * TEMP
      DPARM(5) = DPARM(5) * TEMP
      TEMP = MAX (DPARM(4), DPARM(5)) / (3600.0 * CATR(KRCIC))
      I = TEMP * 6.0
      I = I / 2
      NB = 2 * I + 1
C                                       Copy any header keywords
      CALL BSCKEY (DISK1, SLOT1, DISKO, SLOTO, DPARM(1), IRET)
C                                       set cl box
      NBOXES = XNBOX + 0.1
      CALL FILL (200, 0, CLBOX)
      IF (NBOXES.LE.0) THEN
         NBOXES = 1
         CLBOX(1,1) = 1
         CLBOX(2,1) = 1
         CLBOX(3,1) = NX
         CLBOX(4,1) = NY
      ELSE
         DO 20 I = 1,NBOXES
            CLBOX(1,I) = IROUND (XBOX(1,I))
            CLBOX(2,I) = IROUND (XBOX(2,I))
            CLBOX(3,I) = IROUND (XBOX(3,I))
            CLBOX(4,I) = IROUND (XBOX(4,I))
            IF (CLBOX(1,I).GT.0) THEN
               CLBOX(1,I) = MAX (1, MIN (NX, CLBOX(1,I)-IWIN(1)+ 1))
               CLBOX(2,I) = MAX (1, MIN (NY, CLBOX(2,I)-IWIN(2)+ 1))
               END IF
            CLBOX(3,I) = MAX (1, MIN (NX, CLBOX(3,I)-IWIN(1)+ 1))
            CLBOX(4,I) = MAX (1, MIN (NY, CLBOX(4,I)-IWIN(2)+ 1))
 20         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. ERR=',I5)
      END
      SUBROUTINE BSCSET (NB, BEAM, NX, NY, RESID, COMP, IRET)
C-----------------------------------------------------------------------
C   BSCSET copies the first plane into core and copies the 2nd and 3rd
C   planes to the output file.  It also computes the plus and minus beam
C   patches
C   Inputs:
C      NB       I      X and Y dimension of beam patches
C      NX       I      X dimension of ressidual and component fields
C      NY       I      Y dimension of ressidual and component fields
C   Output:
C      BEAM     R(*)   Beam patches (+, -)
C      RESID    R(*)   Initial residual image (i.e. input dirty image)
C      COMP     R(*)   Component image for + comp - all 0
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NB, NX, NY, IRET
      REAL      BEAM(NB,NB,2), RESID(NX,NY), COMP(NX,NY)
C
      INTEGER   IX, IY, JWIN(4), I3L, I4L, I5L, I6L, I7L, I3U, I4U, I5U,
     *   I6U, I7U, I3, I4, I5, I6, I7, IDEPTH(5), IBLKOF, BIND1, BINDO,
     *   BIND2, IROUND, I
      LOGICAL   FIRST
      REAL      TEMP, CX, CY, CW
      INCLUDE 'BSCLN.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Window for destination file.
      JWIN(1) = 1
      JWIN(2) = 1
      JWIN(3) = NX
      JWIN(4) = NY
      JBUFSZ = 2 * MABFSS
C                                       Init values for loop.
      RMIN = MAX (1.0E20, ABS(CATR(KRDMX)), ABS(CATR(KRDMN)))
      RMAX = -RMIN
      WASBLK = .FALSE.
      I3L = IROUND (BLC(3))
      I3U = IROUND (TRC(3))
      I4L = IROUND (BLC(4))
      I4U = IROUND (TRC(4))
      I5L = IROUND (BLC(5))
      I5U = IROUND (TRC(5))
      I6L = IROUND (BLC(6))
      I6U = IROUND (TRC(6))
      I7L = IROUND (BLC(7))
      I7U = IROUND (TRC(7))
C                                       Loop for all possible planes.
      FIRST = .TRUE.
      DO 100 I7 = I7L,I7U
      DO 100 I6 = I6L,I6U
      DO 100 I5 = I5L,I5U
      DO 100 I4 = I4L,I4U
      DO 100 I3 = I3L,I3U
C                                       Set corner selection.
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
C                                       Block offset for source file.
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IBLKOF = IBLKOF + 1
C                                       Initialize for double buffering
         CALL MINIT ('READ', LUN1, FIND1, CATOLD(KINAX),
     *      CATOLD(KINAX+1), IWIN, BUFF1, JBUFSZ, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'INIT', 'INPUT 1', IRET
            GO TO 990
            END IF
         CALL MINIT ('READ', LUN2, FIND2, CATOLD(KINAX),
     *      CATOLD(KINAX+1), IWIN, BUFF2, JBUFSZ, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'INIT', 'INPUT 2', IRET
            GO TO 990
            END IF
C                                       Find block offset for subimage.
         IDEPTH(1) = I3 - I3L + 1
         IDEPTH(2) = I4 - I4L + 1
         IDEPTH(3) = I5 - I5L + 1
         IDEPTH(4) = I6 - I6L + 1
         IDEPTH(5) = I7 - I7L + 1
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
C
         CALL MINIT ('WRIT', LUNO, FINDO, NX, NY, JWIN, BUFFO, JBUFSZ,
     *      IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'INIT', 'OUTPUT', IRET
            GO TO 990
            END IF
C                                       read all rows
         DO 50 IY = 1,NY
            CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) 'READ', 'INPUT 1', IRET
               GO TO 990
               END IF
            CALL MDISK ('READ', LUN2, FIND2, BUFF2, BIND2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) 'READ', 'INPUT 2', IRET
               GO TO 990
               END IF
            CALL MDISK ('WRIT', LUNO, FINDO, BUFFO, BINDO, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) 'WRITE', 'OUTPUT', IRET
               GO TO 990
               END IF
C                                       Dirty image to core
            IF (FIRST) THEN
               DO 10 IX = 1,NX
                  I = IX - 1
                  IF ((BUFF1(BIND1+I).NE.FBLANK) .AND.
     *               (BUFF2(BIND2+I).NE.FBLANK)) THEN
                     RESID(IX,IY) = BUFF1(BIND1+I) - BUFF2(BIND2+I)
                  ELSE
                     WASBLK = .TRUE.
                     END IF
                  COMP(IX,IY) = 0.0
                  BUFFO(BINDO+I) = 0.0
 10               CONTINUE
C                                       Else copy image
            ELSE
               DO 20 IX = 1,NX
                  I = IX - 1
                  IF ((BUFF1(BIND1+I).NE.FBLANK) .AND.
     *               (BUFF2(BIND2+I).NE.FBLANK)) THEN
                     BUFFO(BINDO+I) = (BUFF1(BIND1+I)+BUFF2(BIND2+I))/2.
                     RMIN = MIN (RMIN, BUFFO(BINDO+I))
                     RMAX = MAX (RMAX, BUFFO(BINDO+I))
                  ELSE
                     WASBLK = .TRUE.
                     END IF
 20               CONTINUE
               END IF
 50         CONTINUE
C                                       Write last buffer.
         CALL MDISK ('FINI', LUNO, FINDO, BUFFO, BINDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'FINISH', 'OUTPUT', IRET
            GO TO 990
            END IF
         FIRST = .FALSE.
 100     CONTINUE
C                                       Plus beam on a pixel
      CX = (NB+1)/2
      CY = CX
      CW = DPARM(4) / (2.0 * SQRT (LOG (2.0)) * 3600.*ABS(CATR(KRCIC)))
      CW = CW * CW
      DO 120 IY = 1,NB
         DO 110 IX = 1,NB
            TEMP = ((IX-CX)**2 + (IY-CY)**2) / CW
            IF (TEMP.LE.10.0) THEN
               BEAM(IX,IY,1) = EXP (-TEMP)
            ELSE
               BEAM(IX,IY,1) = 0.0
               END IF
 110        CONTINUE
 120     CONTINUE
C                                       Minus beam is not on a pixel
      CX = (NB+1)/2
      CY = CX
      CW = DPARM(5) / (2.0 * SQRT (LOG (2.0)) * 3600.*ABS(CATR(KRCIC)))
      CW = CW * CW
      TEMP = DPARM(1) * COS (DPARM(2) * DG2RAD) / ABS (CATR(KRCIC)) /
     *   3600.0
      XOFF = IROUND (TEMP)
      CX = CX + TEMP - XOFF
      TEMP = DPARM(1) * SIN (DPARM(2) * DG2RAD) / ABS (CATR(KRCIC)) /
     *   3600.0
      YOFF = IROUND (TEMP)
      CY = CY + TEMP - YOFF
      DO 140 IY = 1,NB
         DO 130 IX = 1,NB
            TEMP = ((IX-CX)**2 + (IY-CY)**2) / CW
            IF (TEMP.LE.10.0) THEN
               BEAM(IX,IY,2) = EXP (-TEMP) / DPARM(3)
            ELSE
               BEAM(IX,IY,2) = 0.0
               END IF
 130        CONTINUE
 140     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DOING ',A,' ON ',A,' FILE: ERROR',I5)
      END
      SUBROUTINE BSCLEN (NB, BEAM, NX, NY, RESID, COMP, IRET)
C-----------------------------------------------------------------------
C   Does the Hogbom Clean
C   Inputs:
C      NB       I      Beam dimensions
C      BEAM     R(*)   Plus beam (centered)
C      NX       I      X dimension of images
C      NY       I      Y dimension of images
C   In/out:
C      RESID    R(*)   Current residual image
C      COMP     R(*)   Point components
C   Output:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NB, NX, NY, IRET
      REAL      BEAM(NB,NB,2), RESID(NX,NY), COMP(NX,NY)
C
      INTEGER   I, I1, I2, J, J1, J2, ITV, ITER, IROUND, IG, IX, IY, NR,
     *   IOFF, JOFF, JWIN(4)
      REAL      FF, FMIN(3), FMAX(3), FLUX, TEMP, TMIN, TMAX, FTEMP,
     *   PIXR(2)
      LOGICAL   TVSOPN, OKAY
      CHARACTER PREFIX*5
      INCLUDE 'BSCLN.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       open up TV display
      ITV = IROUND (DOTV)
      TVSOPN = .FALSE.
      IF (ITV.GT.0) THEN
         CALL TVOPEN (SCRTCH, IRET)
         IF (IRET.NE.0) ITV = 0
         END IF
      IF (ITV.GT.0) THEN
         TVSOPN = .TRUE.
         CALL YINIT (BUFF1, IRET)
         IG = NGRAY + 1
         IF (IRET.EQ.0) CALL YSLECT ('ONNN', IG, 0, SCRTCH, IRET)
         IG = NGRAY + 3
         IF (IRET.EQ.0) CALL YSLECT ('ONNN', IG, 0, SCRTCH, IRET)
         IF (IRET.NE.0) ITV = 0
         END IF
      ITER = -1
      NR = NB / 2
      FLUX = 0.0
      JWIN(1) = 1
      JWIN(2) = 1
      JWIN(3) = NX
      JWIN(4) = NY
C                                       Clean loop
 10   ITER = ITER + 1
      IF (ITER.LT.NITER) THEN
C                                       difference search
         CALL MAXDIF (NX, NY, RESID, NBOXES, CLBOX, XOFF, YOFF, FMAX)
         FF = FMAX(1)
         IX = IROUND (FMAX(2))
         IY = IROUND (FMAX(3))
C                                       TV display
         IF ((ITV.GT.0) .AND. (MOD(ITER,ITV).EQ.0)) THEN
            CALL MAXMIN (NX, NY, RESID, NBOXES, CLBOX, FMIN, FMAX)
            IF (ITER.EQ.0) THEN
               PIXR(1) = FMIN(1)
               PIXR(2) = FMAX(1)
               END IF
            TMAX = FMAX(1)
            TMIN = FMIN(1)
            IF (TMAX-TMIN.LT.0.333*(PIXR(2)-PIXR(1))) THEN
               PIXR(1) = FMIN(1)
               PIXR(2) = FMAX(1)
               END IF
            TEMP = MAX (ABS(TMAX), ABS(TMIN))
            FTEMP = TEMP
            CALL METSCA (TEMP, PREFIX, OKAY)
            IF (TEMP.NE.0) THEN
               TMAX = TMAX * TEMP / FTEMP
               TMIN = TMIN * TEMP / FTEMP
               END IF
            WRITE (MSGTXT,1010) TMIN, TMAX, PREFIX, CUNITS
            CALL MSGWRT (2)
            TEMP = FLUX
            CALL METSCA (TEMP, PREFIX, OKAY)
            WRITE (MSGTXT,1011) ITER, TEMP, PREFIX, CUNITS
            CALL MSGWRT (2)
            CALL BSCTV (NX, NY, RESID, PIXR, CUNITS, ITER, NBOXES,
     *         CLBOX, IRET)
            IF (IRET.GT.0) ITV = 0
            IF (IRET.LT.0) GO TO 900
            END IF
C                                       Put in component list
         FF = FF * GAIN
C                                       Plus or 1 beam
         COMP(IX,IY) = COMP(IX,IY) + FF
         FLUX = FLUX + FF
C                                       Subtract plus beam
         I = IX
         I1 = MAX (1, I - NR)
         I2 = MIN (NX, I + NR)
         J = IY
         J1 = MAX (1, J - NR)
         J2 = MIN (NY, J + NR)
         IOFF = I - NR - 1
         JOFF = J - NR - 1
         DO 30 J = J1,J2
            DO 20 I = I1,I2
               IF (RESID(I,J).NE.FBLANK) RESID(I,J) = RESID(I,J)
     *            - FF * BEAM(I-IOFF,J-JOFF,1)
 20            CONTINUE
 30         CONTINUE
C                                       Add minus beam
         I = IX + XOFF
         I1 = MAX (1, I - NR)
         I2 = MIN (NX, I + NR)
         J = IY + YOFF
         J1 = MAX (1, J - NR)
         J2 = MIN (NY, J + NR)
         IOFF = I - NR - 1
         JOFF = J - NR - 1
         DO 50 J = J1,J2
            DO 40 I = I1,I2
               IF (RESID(I,J).NE.FBLANK) RESID(I,J) = RESID(I,J)
     *            + FF * BEAM(I-IOFF,J-JOFF,2)
 40            CONTINUE
 50         CONTINUE
         GO TO 10
         END IF
C                                       Time to quit
 900  NITER = ITER
      IF (TVSOPN) CALL TVCLOS (SCRTCH, IRET)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Loading TV from',F9.3,' to',F9.3,1X,A,A)
 1011 FORMAT ('after iteration',I8,' component sum',F9.3,1X,A,A)
      END
      SUBROUTINE BSCTV (NX, NY, IMAGE, FRANG, CUNITS, ITER, NBOXES,
     *   CLBOX, IRET)
C-----------------------------------------------------------------------
C   Displays image on TV - interaction later maybe
C   Inputs:
C      NX       I      X dimension of IMAGE
C      NY       I      Y dimension of IMAGE
C      IMAGE    R(*)   Image
C      FRANG    R(2)   Min,max
C      CUNITS   C*8    Units
C      ITER     I      Iteration
C      NBOXES   I      Number Clean boxes
C      CLBOX    I(*)   Clean boxes in pixels (4,*)
C   Output:
C      IRET    I      Error code: < 0 -> reqest to quit
C-----------------------------------------------------------------------
      INTEGER   NX, NY, ITER, NBOXES, CLBOX(4,*), IRET
      REAL      IMAGE(NX,NY), FRANG(2)
      CHARACTER CUNITS*8
C
      INTEGER   IG, J, I1, I2, J1, J2, IX0, IY0, NP, WIND(4),
     *   CATSAV(256), IB(2), IT(2), I
      REAL      PIXR(2), PMX, PMN, TEMP
      LOGICAL   OKAY
      CHARACTER STRING*64, PREFIX*5
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   ROW(MAXIMG)
C-----------------------------------------------------------------------
      CALL COPY (256, CATBLK, CATSAV)
C                                       corners, pixrange
      PIXR(1) = FRANG(1)
      PIXR(2) = FRANG(2)
      IF (NX.GT.MAXXTV(1)) THEN
         I1 = 1 + (NX-MAXXTV(1)) / 2
         I2 = MIN (NX, I1 + MAXXTV(1) - 1)
         IX0 = 1
      ELSE
         I1 = 1
         I2 = NX
         IX0 = (MAXXTV(1) - NX) / 2.0 + 0.5
         END IF
      IF (NY.GT.MAXXTV(2)) THEN
         J1 = 1 + (NY-MAXXTV(2)) / 2
         J2 = MIN (NY, J1 + MAXXTV(2) - 1)
         IY0 = 1
      ELSE
         J1 = 1
         J2 = NY
         IY0 = (MAXXTV(2) - NY) / 2.0 + 0.5
         END IF
C                                       image catalog header
      CATBLK(IIVOL) = 0
      CATBLK(IICNO) = 0
      CATR(IRRAN) = PIXR(1)
      CATR(IRRAN+1) = PIXR(2)
      CALL CHR2H (2, '  ', 1, CATH(IITRA))
      CATBLK(IIWIN+0) = I1
      CATBLK(IIWIN+1) = J1
      CATBLK(IIWIN+2) = I2
      CATBLK(IIWIN+3) = J2
      CATBLK(IICOR+0) = IX0
      CATBLK(IICOR+1) = IY0
      CATBLK(IICOR+2) = IX0 + I2 - I1
      CATBLK(IICOR+3) = IY0 + J2 - J1
      CALL YCWRIT (1, CATBLK(IICOR), CATBLK, ROW, IRET)
      CALL YHOLD ('ONNN', IRET)
C                                       load TV
      NP = I2 - I1 + 1
      DO 30 J = J1,J2
         CALL ISCALE ('  ', MAXINT, PIXR, NP, 1, IMAGE(I1,J), ROW)
         CALL YIMGIO ('WRIT', 1, IX0, IY0, 0, NP, ROW, IRET)
         IF (IRET.NE.0) GO TO 990
         IY0 = IY0 + 1
 30      CONTINUE
C                                       TV window at present
      CALL YWINDO ('READ', WIND, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       iterations
      WRITE (STRING,1000) ITER
      CALL REFRMT (STRING, ' ', NP)
      IG = NGRAY + 1
      IX0 = (WIND(1) + WIND(3)) / 2 - (CSIZTV(1) * NP) / 2
      IY0 = MIN (CATBLK(IICOR+3)+1+CSIZTV(2)/2, WIND(4) - 2 * CSIZTV(2))
      CALL YCHRW (IG, IX0, IY0, STRING(:NP), ROW, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       PixRange
      IF (ABS(PIXR(1)).GT.ABS(PIXR(2))) THEN
         TEMP = PIXR(1)
         CALL METSCA (TEMP, PREFIX, OKAY)
         PMN = TEMP
         PMX = PIXR(2) * TEMP / PIXR(1)
      ELSE IF (PIXR(2).NE.0.0) THEN
         TEMP = PIXR(2)
         CALL METSCA (TEMP, PREFIX, OKAY)
         PMX = TEMP
         PMN = PIXR(1) * TEMP / PIXR(2)
      ELSE
         PMX = 0.0
         PMN = 0.0
         END IF
      WRITE (STRING,1010) PMN, PMX, PREFIX, CUNITS
      CALL REFRMT (STRING, ' ', NP)
      IX0 = (WIND(1) + WIND(3)) / 2 - (CSIZTV(1) * NP) / 2
      IY0 = MAX (CATBLK(IICOR+1)-(3*CSIZTV(2))/2, WIND(2)+CSIZTV(2)/2)
      CALL YCHRW (IG, IX0, IY0, STRING(:NP), ROW, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       boxes
      IF (NBOXES.GT.0) THEN
         IG = 3 + NGRAY
         CALL YZERO (IG, IRET)
         DO 50 I = 1,NBOXES
            OKAY = CLBOX(1,I).EQ.-1
            IF (OKAY) THEN
               IB(1) = -1
               IB(2) = CLBOX(2,I)
            ELSE
               IB(1) = CATBLK(IICOR) + CLBOX(1,I) - CATBLK(IIWIN)
               IB(2) = CATBLK(IICOR+1) + CLBOX(2,I) - CATBLK(IIWIN+1)
               END IF
            IT(1) = CATBLK(IICOR) + CLBOX(3,I) - CATBLK(IIWIN)
            IT(2) = CATBLK(IICOR+1) + CLBOX(4,I) - CATBLK(IIWIN+1)
            CALL DRBOXS (IG, CATBLK(IICOR), OKAY, 0, IB, IT, ROW, IRET)
            IF (IRET.NE.0) GO TO 990
 50         CONTINUE
         END IF
C
      CALL YHOLD ('OFFF', IRET)
 990  CALL COPY (256, CATSAV, CATBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ITERATION',I12)
 1010 FORMAT ('PIXRANGE',2F9.3,1X,A,A)
      END
      SUBROUTINE MAXMIN (NX, NY, IMAGE, NW, WINS, FMIN, FMAX)
C-----------------------------------------------------------------------
C   Computes max and min over windows into image
C   Inputs:
C      NX      I      X dimension of IMAGE
C      NY      I      Y dimension of IMAGE
C      IMAGE   R(*)   Image
C      NW      I      Number windows
C      WINS    I(*)   Windows(4,NW)
C   Output
C      FMIN    R(3)   Min, x, y
C      FMAX    R(3)   Max, x, y
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NW, WINS(4,*)
      REAL      IMAGE(NX,NY), FMIN(3), FMAX(3)
C
      INTEGER   I, J, IW, I1, I2, J1, J2, R, RR
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       loop over windows
      FMIN(1) = 0.0
      FMAX(1) = 0.0
      DO 100 IW = 1,NW
C                                       rectangles
         IF (WINS(1,IW).GT.0) THEN
            I1 = WINS(1,IW)
            I2 = WINS(3,IW)
            J1 = WINS(2,IW)
            J2 = WINS(4,IW)
            DO 20 J = J1,J2
               DO 10 I = I1,I2
                  IF (IMAGE(I,J).NE.FBLANK) THEN
                     IF (IMAGE(I,J).GT.FMAX(1)) THEN
                        FMAX(1) = IMAGE(I,J)
                        FMAX(2) = I
                        FMAX(3) = J
                     ELSE IF (IMAGE(I,J).LT.FMIN(1)) THEN
                        FMIN(1) = IMAGE(I,J)
                        FMIN(2) = I
                        FMIN(3) = J
                        END IF
                     END IF
 10               CONTINUE
 20            CONTINUE
C                                       circles
         ELSE
            I1 = MAX (1, WINS(3,IW) - WINS(2,IW))
            J1 = MAX (1, WINS(4,IW) - WINS(2,IW))
            I2 = MIN (NX, WINS(3,IW) + WINS(2,IW))
            J2 = MIN (NY, WINS(4,IW) + WINS(2,IW))
            RR = WINS(2,IW) * WINS(2,IW)
            DO 40 J = J1,J2
               DO 30 I = I1,I2
                  R = (I-WINS(3,IW))*(I-WINS(3,IW)) +
     *               (J-WINS(4,IW))*(J-WINS(4,IW))
                  IF ((R.LE.RR) .AND. (IMAGE(I,J).NE.FBLANK)) THEN
                     IF (IMAGE(I,J).GT.FMAX(1)) THEN
                        FMAX(1) = IMAGE(I,J)
                        FMAX(2) = I
                        FMAX(3) = J
                     ELSE IF (IMAGE(I,J).LT.FMIN(1)) THEN
                        FMIN(1) = IMAGE(I,J)
                        FMIN(2) = I
                        FMIN(3) = J
                        END IF
                     END IF
 30               CONTINUE
 40            CONTINUE
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MAXDIF (NX, NY, IMAGE, NW, WINS, XO, YO, FMAX)
C-----------------------------------------------------------------------
C   Computes max abs(diff) of plus - minus over windows into image
C   Inputs:
C      NX      I      X dimension of IMAGE
C      NY      I      Y dimension of IMAGE
C      IMAGE   R(*)   Image
C      NW      I      Number windows
C      WINS    I(*)   Windows(4,NW)
C      XO      I      Offset in X pixels of minus beam (plus other sign)
C      YO      I      Offset in Y pixels of minus beam
C   Output
C      FMAX    R(3)   Max, x, y
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NW, WINS(4,*), XO, YO
      REAL      IMAGE(NX,NY), FMAX(3)
C
      INTEGER   I, J, IW, I1, I2, J1, J2, R, RR, XMAX, XMIN, YMAX, YMIN,
     *   IP, JP, IM, JM
      REAL      TMAX, TMIN, TEMP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       loop over windows
      TMIN = 0.0
      TMAX = 0.0
      DO 100 IW = 1,NW
C                                       rectangles
         IF (WINS(1,IW).GT.0) THEN
            I1 = WINS(1,IW)
            I2 = WINS(3,IW)
            J1 = WINS(2,IW)
            J2 = WINS(4,IW)
            DO 20 J = J1,J2
               JP = J - YO/2
               JM = JP + YO
               IF ((JP.GE.1) .AND. (JM.GE.1) .AND. (JP.LE.NY) .AND.
     *            (JM.LE.NY)) THEN
                  DO 10 I = I1,I2
                     IP = I - XO/2
                     IM = IP + XO
                     IF ((IP.GE.1) .AND. (IM.GE.1) .AND. (IP.LE.NX)
     *                  .AND. (IM.LE.NX)) THEN
                        IF ((IMAGE(IP,JP).NE.FBLANK) .AND.
     *                     (IMAGE(IM,JM).NE.FBLANK)) THEN
                           TEMP = IMAGE(IP,JP) - IMAGE(IM,JM)
                           IF (TEMP.GT.TMAX) THEN
                              TMAX = TEMP
                              XMAX = IP
                              YMAX = JP
                           ELSE IF (TEMP.LT.TMIN) THEN
                              TMIN = TEMP
                              XMIN = IP
                              YMIN = JP
                              END IF
                           END IF
                        END IF
 10                  CONTINUE
                  END IF
 20            CONTINUE
C                                       circles
         ELSE
            I1 = MAX (1, WINS(3,IW) - WINS(2,IW))
            J1 = MAX (1, WINS(4,IW) - WINS(2,IW))
            I2 = MIN (NX, WINS(3,IW) + WINS(2,IW))
            J2 = MIN (NY, WINS(4,IW) + WINS(2,IW))
            RR = WINS(2,IW) * WINS(2,IW)
            DO 40 J = J1,J2
               JP = J - YO/2
               JM = JP + YO
               IF ((JP.GE.1) .AND. (JM.GE.1) .AND. (JP.LE.NY) .AND.
     *            (JM.LE.NY)) THEN
                  DO 30 I = I1,I2
                     IP = I - XO/2
                     IM = IP + XO
                     R = (I-WINS(3,IW))*(I-WINS(3,IW)) +
     *                  (J-WINS(4,IW))*(J-WINS(4,IW))
                     IF ((IP.GE.1) .AND. (IM.GE.1) .AND. (IP.LE.NX)
     *                  .AND. (IM.LE.NX) .AND. (R.LE.RR)) THEN
                        IF ((IMAGE(IP,JP).NE.FBLANK) .AND.
     *                     (IMAGE(IM,JM).NE.FBLANK)) THEN
                           TEMP = IMAGE(IP,JP) - IMAGE(IM,JM)
                           IF (TEMP.GT.TMAX) THEN
                              TMAX = TEMP
                              XMAX = IP
                              YMAX = JP
                           ELSE IF (TEMP.LT.TMIN) THEN
                              TMIN = TEMP
                              XMIN = IP
                              YMIN = JP
                              END IF
                           END IF
                        END IF
 30                  CONTINUE
                  END IF
 40            CONTINUE
            END IF
 100     CONTINUE
      IF (ABS(TMIN).GT.TMAX) THEN
         FMAX(1) = TMIN / 2.0
         FMAX(2) = XMIN
         FMAX(3) = YMIN
      ELSE
         FMAX(1) = TMAX / 2.0
         FMAX(2) = XMAX
         FMAX(3) = YMAX
         END IF
C
 999  RETURN
      END
      SUBROUTINE BSCOUT (NB, BEAM, NX, NY, RESID, COMP, IRET)
C-----------------------------------------------------------------------
C   Convolves the component image offset and adds it to the residual
C   image, writing out the final image plane
C   Inputs:
C      NB       I      Size of beam array
C      NX       I      X dimension of residual and component
C      NY       I      Y dimension of residual and component
C      COMP     R(*)   Clean component image
C   In/out:
C      RESID    R(*)   Residual image in, final image out
C   Output:
C      BEAM     R(*)   Clean beam image (offset)
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NB, NX, NY, IRET
      REAL      BEAM(NB,NB), RESID(NX,NY), COMP(NX,NY)
C
      INTEGER   IX, IY, I, I1, I2, J, J1, J2, NR, IC, JC, JWIN(4),
     *   IBLKOF, BINDO, IOFF, JOFF, XOFF1, YOFF1, IROUND
      REAL      TEMP, CW, CX, CY, FLUX, FMAX(3), FMIN(3), PIXR(2)
      CHARACTER PREFIX*5
      LOGICAL   OKAY
      INCLUDE 'BSCLN.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      FLUX = 0.0
      IF (BMAJ.GT.0.0) THEN
C                                       compute offset beam
         CW = BMAJ / (2.0 * SQRT (LOG (2.0)) * 3600.*ABS (CATR(KRCIC)))
         CW = CW * CW
         TEMP = 0.5 * DPARM(1) * COS (DPARM(2)*DG2RAD) / 3600. /
     *      ABS (CATR(KRCIC))
         XOFF1 = IROUND (TEMP)
         CX = (NB+1)/2
         CX = CX + TEMP - XOFF1
         TEMP = 0.5 * DPARM(1) * SIN (DPARM(2)*DG2RAD) / 3600. /
     *      ABS (CATR(KRCIC))
         YOFF1 = IROUND (TEMP)
         CY = (NB+1)/2
         CY = CY + TEMP - YOFF1
         DO 20 IY = 1,NB
            DO 10 IX = 1,NB
               TEMP = ((IX-CX)**2 + (IY-CY)**2) / CW
               IF (TEMP.LE.10.0) THEN
                  BEAM(IX,IY) = EXP (-TEMP)
               ELSE
                  BEAM(IX,IY) = 0.0
                  END IF
 10            CONTINUE
 20         CONTINUE
C                                       convolve components to resid
         NR = NB / 2
         DO 50 IY = 1,NY
            DO 45 IX = 1,NX
               IF (COMP(IX,IY).NE.0.0) THEN
                  FLUX = FLUX + COMP(IX,IY)
                  IC = IX + XOFF1
                  I1 = IC - NR
                  I2 = IC + NR
                  IOFF = IC - NR - 1
                  JC = IY + YOFF1
                  J1 = JC - NR
                  J2 = JC + NR
                  JOFF = JC - NR - 1
                  I1 = MAX (1, I1)
                  I2 = MIN (NX, I2)
                  J1 = MAX (1, J1)
                  J2 = MIN (NY, J2)
                  DO 40 J = J1,J2
                     DO 30 I = I1,I2
                        RESID(I,J) = RESID(I,J) +
     *                     COMP(IX,IY) * BEAM(I-IOFF,J-JOFF)
 30                     CONTINUE
 40                  CONTINUE
                  END IF
 45            CONTINUE
 50         CONTINUE
         END IF
C                                       info
      TEMP = FLUX
      CALL METSCA (FLUX, PREFIX, OKAY)
      WRITE (MSGTXT,1100) FLUX, PREFIX, CUNITS
      CALL MSGWRT (4)
      JWIN(1) = 1
      JWIN(2) = 1
      JWIN(3) = NX
      JWIN(4) = NY
      CALL MAXMIN (NX, NY, RESID, 1, JWIN, FMIN, FMAX)
      PIXR(1) = FMIN(1)
      PIXR(2) = FMAX(1)
      RMAX = MAX (RMAX, FMAX(1))
      RMIN = MIN (RMIN, FMIN(1))
      FLUX = MAX (ABS(FMAX(1)), ABS(FMIN(1)))
      TEMP = FLUX
      IF (TEMP.NE.0.0) THEN
         CALL METSCA (TEMP, PREFIX, OKAY)
         FMAX(1) = FMAX(1) * TEMP/FLUX
         FMIN(1) = FMIN(1) * TEMP/FLUX
      ELSE
         PREFIX = ' '
         END IF
      WRITE (MSGTXT,1105) FMIN(1), FMAX(1), PREFIX, CUNITS
      CALL MSGWRT (4)
      IF (DOTV.GT.0.5) THEN
         CALL TVOPEN (SCRTCH, IRET)
         IF (IRET.EQ.0) THEN
            IY = NGRAY + 1
            CALL YZERO (IY, IRET)
            CALL BSCTV (NX, NY, RESID, PIXR, CUNITS, NITER, NBOXES,
     *         CLBOX, IRET)
            CALL TVCLOS (SCRTCH, IRET)
            END IF
         END IF
C                                       write it out
C                                       Window for destination file.
      IBLKOF = 1
      CALL MINIT ('WRIT', LUNO, FINDO, NX, NY, JWIN, BUFFO, JBUFSZ,
     *   IBLKOF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1110) 'INIT', 'OUTPUT', IRET
         GO TO 990
         END IF
C                                       read all rows
      DO 120 IY = 1,NY
         CALL MDISK ('WRIT', LUNO, FINDO, BUFFO, BINDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) 'WRITE', 'OUTPUT', IRET
            GO TO 990
            END IF
C                                       Clean image to disk
         CALL RCOPY (NX, RESID(1,IY), BUFFO(BINDO))
 120     CONTINUE
C                                       Write last buffer.
      CALL MDISK ('FINI', LUNO, FINDO, BUFFO, BINDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1110) 'FINISH', 'OUTPUT', IRET
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Sum of components',F9.3,1X,A,A)
 1101 FORMAT ('Sum plus, minus components',2F9.3,1X,A,A)
 1105 FORMAT ('Output image min',F9.3,' max',F9.3,1X,A,A)
 1110 FORMAT ('DOING ',A,' ON ',A,' FILE: ERROR',I5)
      END
      SUBROUTINE BSCHIS
C-----------------------------------------------------------------------
C     BSCHIS writes history file, copies ext files
C-----------------------------------------------------------------------
C
      CHARACTER HILINE*72, NOTTYP*2
      INTEGER   IH1LUN, IH2LUN, IBLC(7), ITRC(7), IROUND, I, IRET
      INCLUDE 'BSCLN.INC'
      DATA NOTTYP /'  '/
      DATA IH1LUN, IH2LUN /27, 28/
C-----------------------------------------------------------------------
      CATR(KRDMN) = RMIN
      CATR(KRDMX) = RMAX
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
C                                       Initialize HITAB
      CALL HIINIT (3)
C                                       Create and copy history file.
      CALL HISCOP (IH1LUN, IH2LUN, DISK1, DISKO, SLOT1, SLOTO,
     *   CATBLK, BUFF1, BUFFO, IRET)
      IF (IRET.GT.3) GO TO 200
      IF (IRET.EQ.3) GO TO 100
C                                       Add BSCLN history.
      CALL HENCO1 (TSKNAM, NAME1, CLAS1, SEQ1, DISK1, IH2LUN, BUFFO,
     *   IRET)
      IF (IRET.NE.0) GO TO 100
      CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, IH2LUN, BUFFO,
     *   IRET)
      IF (IRET.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLASO, SEQO, DISKO, IH2LUN, BUFFO,
     *   IRET)
      IF (IRET.NE.0) GO TO 100
      DO 10 I = 1,7
         IBLC(I) = IROUND (BLC(I))
         ITRC(I) = IROUND (TRC(I))
 10      CONTINUE
      WRITE (HILINE,1000) 'BLC', IBLC
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,1000) 'TRC', ITRC
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       DPARMs
      WRITE (HILINE,1010) TSKNAM, DPARM(1)
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,1011) TSKNAM, DPARM(2)
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,1012) TSKNAM, DPARM(3)
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,1013) TSKNAM, DPARM(4)
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,1014) TSKNAM, DPARM(5)
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       clean parameters
      WRITE (HILINE,1020) TSKNAM, NITER
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,1021) TSKNAM, GAIN
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
      IF (XFLUX.GT.0.0) THEN
         WRITE (HILINE,1022) TSKNAM, XFLUX
         CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
      WRITE (HILINE,1023) TSKNAM, BMAJ
      CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
      IF (IRET.NE.0) GO TO 100
      DO 30 I = 1,NBOXES
         WRITE (HILINE,1025) TSKNAM, I, CLBOX(1,I), CLBOX(2,I),
     *      CLBOX(3,I), CLBOX(4,I)
         CALL HIADD (IH2LUN, HILINE, BUFFO, IRET)
         IF (IRET.NE.0) GO TO 100
 30      CONTINUE
C
 100  CALL HICLOS (IH2LUN, .TRUE., BUFFO, IRET)
C                                       Copy CC files and others
 200  CALL ALLTAB (0, NOTTYP, IH1LUN, IH2LUN, DISK1, DISKO, SLOT1,
     *   SLOTO, CATBLK, BUFF1, BUFFO, IRET)
C                                       Successful finish
 310  CALL MAPCLS ('READ', DISK1, SLOT1, LUN1, FIND1, CATOLD, .FALSE.,
     *   BUFFO, IRET)
      CALL MAPCLS ('INIT', DISKO, SLOTO, LUNO, FINDO, CATBLK, .TRUE.,
     *   BUFFO, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BSCLN ',A,' =',6(I5,','),I5)
 1010 FORMAT (A,'BSTHROW =',F9.3,' / BS throw in arcsec')
 1011 FORMAT (A,'BSROTATE =',F8.3,' / BS rotation degrees')
 1012 FORMAT (A,'BSRATIO =',F9.5,' / BS +/- gain')
 1013 FORMAT (A,'BSPBEAM =',F9.2,' / BS + beam fwhm asec')
 1014 FORMAT (A,'BSMBEAM =',F9.2,' / BS - beam fwhm asec')
 1020 FORMAT (A,'NITER   =',I9,' / BS clean iterations')
 1021 FORMAT (A,'GAIN    =',F9.4,' / BS clean gain')
 1022 FORMAT (A,'FLUX    =',F9.4,' / BS clean flux limit')
 1023 FORMAT (A,'BMAJ    =',F9.3,' / BS beam fwhm asec')
 1025 FORMAT (A,'CLBOX(*,',I2,')=',3(I5,','),I5,' / BS clean window')
      END
      SUBROUTINE BSCKEY (DISKIN, CNOIN, DISKO, CNOOUT, TOTHRO, IERR)
C-----------------------------------------------------------------------
C   Copy any file catalog keyword/value pairs from one file to another
C   except the BSTHROW is replaced by TOTHROW.
C   Inputs:
C      DISKIN  I  Input file disk number.
C      CNOIN   I  Input file catalog slot number
C      DISKO   I  Output file disk number.
C      CNOOUT  I  Output file catalog slot number
C   Outputs:
C      IERR    I  Error code, 0=OK else CATKEY error.
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, DISKO, CNOOUT, IERR
      REAL      TOTHRO
C
      INTEGER   XMXKEY
C                                       XMXKEY = max number of keywords
      PARAMETER (XMXKEY=100)
      CHARACTER KEYWRD(XMXKEY)*8
      INTEGER NUMKEY, LOCS(XMXKEY), KEYTYP(XMXKEY), VALUES(2*XMXKEY),
     *   BUFFER(256), I
C-----------------------------------------------------------------------
C                                       Input and output the same?
      IF ((DISKIN.EQ.DISKO) .AND. (CNOIN.EQ.CNOOUT)) GO TO 999
C                                       Read old keyword/value pairs
      NUMKEY = XMXKEY
      CALL CATKEY ('ALL ', DISKIN, CNOIN, KEYWRD, NUMKEY, LOCS, VALUES,
     *   KEYTYP, BUFFER, IERR)
C                                       Write to output file
      IF ((IERR.EQ.0) .AND. (NUMKEY.GT.0)) THEN
         DO 10 I = 1,NUMKEY
            IF (KEYWRD(I).NE.'BSTHROW') CALL CATKEY ('WRIT', DISKO,
     *         CNOOUT, KEYWRD(I), 1, LOCS(I), VALUES, KEYTYP(I),
     *         BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
 10         CONTINUE
         LOCS(1) = 1
         CALL RCOPY (1, TOTHRO, VALUES(1))
         KEYTYP(1) = 2
         KEYWRD(1) = 'TOTHROW'
         CALL CATKEY ('WRIT', DISKO, CNOOUT, KEYWRD(1), 1, LOCS(1),
     *      VALUES, KEYTYP(1), BUFFER, IERR)
         END IF
C
 999  RETURN
      END

