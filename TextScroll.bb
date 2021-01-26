; 1)  89 x 89 = ?????, corners
; 5)  285 x 169 = 35062 tiles, corners
; 6)  253 x 197 = 35898 tiles, centers
; 7)  233 x 233 = 38130 tiles, corners
; 8)  197 x 285 = 39066 tiles, centers
; 9)  253 x 253 = 43010 tiles, corners
; 10) 285 x 285 = 51650 tiles, centers
Global sx=89
Global sy=89


Global test_layout = 0

Dim maze(0,0) ;dummy to keep maze data in global
Dim getcos(3),getsin(3)
Global maze_boxes = 0

getcos(0)=1
getcos(2)=-1
getsin(1)=1
getsin(3)=-1


; create sin&cos table
Dim sin_tb#(1079)
Dim cos_tb#(1079)
For i=0 To 1079
	sin_tb(i)=Sin(i)
	cos_tb(i)=Cos(i)
Next

; Define the type for each stars in a starfield
Type star_info
  Field hand
  Field xpos#
  Field ypos#
  Field zpos#
  Field velocity#
End Type


; Define a type for scrolltext-cubes: entityhandle, x- and y position.
Type cube_info
  Field hand
  Field xpos#
  Field ypos#
End Type

Type cube_info2
  Field hand
  Field xpos#
  Field ypos#
  Field zpos#
End Type

; for now, it's only used for walls
Type object_info
  Field hand
  Field xpos#
  Field ypos#
  Field zpos#
  Field object_type ; this is yet unused
End Type

;---------------------------------------------------------------
; Define a type for scrolltext-cubes: entityhandle, x- and y position.
Type cube_info_grid
  Field hand
  Field xpos#
  Field ypos#
  Field zpos#
  Field mx
  Field my
  Field movement#
End Type

; this is the per-player data
Type game_data
     Field player_x
     Field player_y
     Field out_of_body
     Field drop_objects
End Type

; used for the maze matrix
Global player_x, player_y

Global out_of_body = 0


If test_layout Then
   Graphics 1600,1200,0,0
   Text 1150,0,"boxes="
   init_maze(sx,sy,1)
   WaitKey
   End
EndIf


;----------------------------------------------------------------------------------------
; This is where we move to 3D rendering
;----------------------------------------------------------------------------------------
SetGfx()

SetBuffer BackBuffer()

; create an empty texture
Global tex_copyscreen = CreateTexture(GraphicsWidth(), GraphicsHeight(), 256)

; this is texture area for playing around
Global texture_buffer = CreateImage (64, 68)

; Creating an array of textures for the font

fontname$="Courrier New"
Global fnt=LoadFont(fontname$,170,True,False,False)
If Not fnt Then Print "missing '"+fontname$+"' font":End

fontname$="Courrier"
Global fnt2=LoadFont(fontname$,14,True,False,False)
If Not fnt2 Then Print "missing '"+fontname$+"' font":End


Dim letter(96)
ClsColor 128,255,64
SetFont fnt
For i=0 To 96
  letter(i)=CreateTexture (128,128, 256)
  SetBuffer TextureBuffer(letter(i))
  Cls
  Color 255, 64, 128
  Text 62,58,Chr$(32+i),True,True
Next


Dim fxtexture(11)

fxtexture(1)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(1))
	Color 64, 128, 255
	Rect 0, 0, 64, 64
	Color 255, 128, 64
	Rect 0, 0, 32, 32
	Rect 32, 32, 32, 32

fxtexture(2)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(2))
	Color 255, 0, 0
	Rect 0, 0, 64, 64
	Color 0, 255, 0
	Rect 0, 0, 32, 32
	Rect 32, 32, 32, 32

fxtexture(3)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(3))
	Color 0, 0, 255
	Rect 0, 0, 64, 64
	Color 255, 255, 0
	Rect 0, 0, 32, 32
	Rect 32, 32, 32, 32

fxtexture(4)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(4))
	Color 255, 0, 255
	Rect 0, 0, 64, 64
	Color 0, 255, 255
	Rect 0, 0, 32, 32
	Rect 32, 32, 32, 32

fxtexture(5)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(5))
	Color 128, 64, 255
	Rect 0, 0, 64, 64
	Color 255, 128, 64
	Oval 0, 0, 64, 64

fxtexture(6)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(6))
	Color 128, 255, 64
	Rect 0, 0, 64, 64
	Color 128, 64, 255
	Oval 0, 0, 64, 64

fxtexture(7)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(7))
	Color 0, 255, 0
	Rect 0, 0, 64, 64
	Color 255, 0, 0
	Oval 0, 0, 64, 64

fxtexture(8)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(8))
	Color 0, 0, 255
	Rect 0, 0, 64, 64
	Color 0, 255, 0
	Oval 0, 0, 64, 64

fxtexture(9)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(9))
	Color 255, 255, 255
	Rect 0, 0, 64, 64
	
	Color 128, 128, 128
	Rect 0, 0, 32, 32
	
   	Color 64, 64, 64
	Rect 32, 32, 32, 32

fxtexture(10)	= CreateTexture (64, 64, 256)


fxtexture(11)	= CreateTexture (64, 64, 256)
SetBuffer TextureBuffer (fxtexture(11))
ScaleTexture fxtexture(11),1,0.1
	Color 32, 128, 64
	Rect 0, 0, 64, 64
	Color 128, 32, 64
	Rect 0, 0, 32, 32
	Rect 32, 32, 32, 32


AmbientLight 64,64,64

Global ground	= CreatePlane (16)
EntityTexture ground, fxtexture(9),0,1
;MoveEntity ground, 0, -1, 0
EntityType ground,3
EntityAlpha ground,0.75

; Define the Lighting
lumiere1=CreateLight(1)
;lumiere2=CreateLight(2)

LightColor lumiere1,255,64,127
;LightColor lumiere2,127,64,255

MoveEntity lumiere1,-75,75,75
;MoveEntity lumiere2,50,50,-50

LightRange lumiere1,100
;LightRange lumiere2,100


PointEntity lumiere1, ground
;PointEntity lumiere2, ground

Global camera=CreateCamera()
EntityType camera,4
EntityRadius camera,2
CameraViewport camera,0, 0, GraphicsWidth (), GraphicsHeight ()


; determines camera movement
Global cam_movex# = 0
Global cam_movey# = 0
Global cam_movez# = 0
Global cam_posx#  = 0
Global cam_posy#  = 2
Global cam_posz#  = -100

Global cam_viewport_pitch# = 0
Global cam_viewport_yaw# = 0

PositionEntity camera,cam_posx#,cam_posy#,cam_posz#

mirror=CreateMirror()




;    EntityTexture matrice\hand, btex

;For matrice.cube_info2=Each cube_info2      ;Update entity-positions And Delete If necessary...
;    EntityFX matrice\hand,2
;    PositionEntity matrice\hand, matrice\xpos#, matrice\ypos#, matrice\zpos#

;Next

;
Global scrolltext$="FIND ALL SECRETS AND COME BACK...   "

Global cuberoom=0             ; this flag indicates if the cuberoom is built or not (1 or 0)

Global newscrollitem=0        ;Delaycounter for next scrolltext-cube
Global scrollcycle=0
Global sc_offset=1            ;Offset for relevant character of scrolltext$
Global flp=1                  ;switch for only rendering texture every second loop
Global HorizontalSpeed# = 0.1 ;This is the speed in wich the player moves
Global mouse_speed# = 0.15

SeedRnd MilliSecs()

Global groundglue# = 0

; These are the wall unit variables
Dim wall_texture_number(5)
Dim wall_rotate(5)
Dim wall_angle(5)
For i = 1 To 5
   wall_angle(i) = 0
   wall_rotate(i) = 0
Next

Global fx_direction# = +0.001
Global fx_u# = + 0.01
Global fx_v# = + 0.2
Dim walleffect(8)
Dim wallfxzoom(8)
Dim walldisplace#(8)


; this is the super planetlike sphere
Global huge_sphere = CreateSphere(32)
ScaleMesh huge_sphere,250,250,250
PositionEntity huge_sphere,0,500,0
EntityType huge_sphere,3
EntityTexture huge_sphere,tex_copyscreen,0,1

; this sphere rotates around
Global rotating_sphere = CreateSphere(32)
ScaleMesh rotating_sphere,75,75,75
EntityTexture rotating_sphere, fxtexture(10),0,1
EntityType rotating_sphere,3

Dim sphere_direction#(5)
Dim sphere_height#(5)
For i = 1 To 5
   sphere_height#(i) = 55
   sphere_direction#(i) = 0
Next

Global sphere1= CreateSphere(32)
ScaleMesh sphere1,100,100,100
PositionEntity sphere1,500,55,500
EntityType sphere1,3

Global sphere2= CreateSphere(32)
ScaleMesh sphere2,100,100,100
PositionEntity sphere2,-500,55,500
EntityType sphere2,3

Global sphere3= CreateSphere(32)
ScaleMesh sphere3,100,100,100
PositionEntity sphere3,-500,55,-500
EntityType sphere3,3

Global sphere4= CreateSphere(32)
ScaleMesh sphere4,100,100,100
PositionEntity sphere4,500,55,-500
EntityType sphere4,3


Collisions 4,3,2,2



player_default.game_data = New game_data
     player_x = 0
     player_y = 10
     out_of_body = 0
     drop_objects = 0

; this is where we build the maze
init_maze(sx,sy,1)



main()

;FreeSound musc
ClearWorld() 
End


;----------------------------------------------------------------------------------------
; Main Loop
;----------------------------------------------------------------------------------------
Function main()

;    Print "scrolltext"
;    Print

SetupWorld()

While Not KeyHit(1)


    LocalControls()

    AnimatingStarfield()

    RotatingSphere()

    SphereAnimate()

    CubeRoomHandle()

    If Flp=1 Then UpdateTexture(10)                  ;updatetexture() will therefor only be called every 2nd loop 
 ;   PositionTexture fxtexture(11),sin_tb#(scrollcycle) * 2,0

    makemaze_3D((sx+1)/2,(sy+1)/2,0,0,(sx+1),(sy+1),20)

    WallUnit()

    UpdateWorld
    RenderWorld
  
    CopyRect 0,0,GraphicsWidth(), GraphicsHeight(),-1,-1,BackBuffer(),TextureBuffer(tex_copyscreen)

	If KeyDown (70) Then
 	   SetFont fnt2
       Color 255, 255, 255
       Text 0,0,"posx: " + Int(cam_posx#)
       Text 0,10,"posy: " + Int(cam_posy#)
       Text 0,20,"posz: " + Int(cam_posz#)

       Text 100,0,"maze x: " + player_x
       Text 100,10,"maze y: " + player_y
       Text 0,50,"Triangles Rendered: "+TrisRendered() 
    EndIf

    Flip

    UpdateCounters()
Wend
End Function



Function UpdateCounters()
    flp=1-flp     ;flp switches between 1 and 0 (1-1=0 -> 1-0=1)

    scrollcycle = scrollcycle + 1
    If scrollcycle > 360 Then scrollcycle = 1

End Function


;----------------------------------------------------------------------------------------
Function LocalControls()
;Global cam_movey# = 0
;Global cam_movez# = 0


    ; ******************************************* Mouse...
    cam_viewport_pitch# = cam_viewport_pitch# + MouseYSpeed() * mouse_speed#
    If cam_viewport_pitch# < -85 Then cam_viewport_pitch# = -85
    If cam_viewport_pitch# > +85 Then cam_viewport_pitch# = +85

    TurnEntity Camera, 0,  -MouseXSpeed() * mouse_speed#,0
    cam_viewport_yaw# = EntityYaw#(camera)
    RotateEntity Camera, cam_viewport_pitch#, cam_viewport_yaw#, 0

    MoveMouse GraphicsWidth()/2, GraphicsHeight()/2


    ; ******************************************* Keys...
    If KeyDown (42) Or KeyDown(54) Then
       HorizontalSpeed# = 1.25
    Else
       If HorizontalSpeed# > 0.15 Then
          HorizontalSpeed# = HorizontalSpeed - 0.02
       Else
          HorizontalSpeed# = 0.15
       EndIf
    EndIf

    If KeyHit (6) Then
       SetLiftingSphere (1)
    EndIf

    If KeyHit (7) Then
       SetLiftingSphere (2)
    EndIf

    If KeyHit (8) Then
       SetLiftingSphere (3)
    EndIf

    If KeyHit (9) Then
       SetLiftingSphere (4)
    EndIf


    If KeyHit (2) Then
       SetRotatingWall(1)
    EndIf

    If KeyHit (3) Then
       SetRotatingWall(2)
    EndIf

    If KeyHit (4) Then
       SetRotatingWall(3)
    EndIf

    If KeyHit (5) Then
       SetRotatingWall(4)
    EndIf

    If KeyHit (28) Then
       SetRotatingWall(1)
       SetRotatingWall(2)
       SetRotatingWall(3)
       SetRotatingWall(4)
    EndIf

    If KeyHit (59) Then
       AOB_experience(1)
    EndIf

    If KeyHit (60) Then
       AOB_experience(2)
    EndIf

    If KeyHit (61) Then
       AOB_experience(3)
    EndIf

    If KeyHit (62) Then
       AOB_experience(4)
    EndIf


    Select out_of_body

       Case 0
          body_movement()
       Case 1
          PositionEntity camera, sin_tb#(scrollcycle+90) * 475, 510 + sin_tb#(scrollcycle) * 300, cos_tb#(scrollcycle+90) * 475
       Case 2
          PositionEntity camera, cos_tb#(scrollcycle+90) * 475, 510 + sin_tb#(scrollcycle) * 300, sin_tb#(scrollcycle+90) * 475
       Case 3
          PositionEntity camera, cos_tb#(scrollcycle+270) * 475, 510 + cos_tb#(scrollcycle) * 300, cos_tb#(scrollcycle) * 475
       Case 4
          PositionEntity camera, sin_tb#(scrollcycle+270) * 475, 510 + sin_tb#(scrollcycle) * 300, cos_tb#(scrollcycle) * 475

    End Select

End Function


;----------------------------------------------------------------------------------------
Function AOB_experience(experience_number)
       If Not in_center_room() Then
          If sphere_height# (experience_number) > 300 Then
             If out_of_body <> experience_number Or out_of_body = 0 Then
                out_of_body = experience_number
             Else
                out_of_body = 0
             EndIf
          EndIf
       EndIf
End Function


;----------------------------------------------------------------------------------------
Function in_center_room()
   If ((cam_posx# < 202 And cam_posx# > -202) And (cam_posz# < 202 And cam_posz# > -202)) Then
      Return 1
   EndIf
   Return 0
End Function

;----------------------------------------------------------------------------------------
Function far_from_room()
   If Not ((cam_posx# < 400 And cam_posx# > -400) And (cam_posz# < 400 And cam_posz# > -400)) Then
      Return 1
   EndIf
   Return 0
End Function



;----------------------------------------------------------------------------------------
Function body_movement()

    ; this is for jumping
   ; If KeyHit (57) And groundglue#=1 Then
    If KeyHit (57) Then
       groundglue#=0
       cam_movey#=HorizontalSpeed#*2
    EndIf

    ; this is for sidewalking
	If KeyDown (203) Then
	   cam_movex# = HorizontalSpeed#
	   groundglue#=0
	
	ElseIf KeyDown (205) Then
	   cam_movex# = -HorizontalSpeed#
	   groundglue#=0
	
    Else
	   cam_movex# = 0
	
    EndIf

    ; this is for foreward and backward
	If KeyDown (200) Then
	   cam_movez# = HorizontalSpeed#
	   groundglue#=0
	
	ElseIf KeyDown (208) Then
	   cam_movez# = -HorizontalSpeed#
	   groundglue#=0
	
	Else
	   cam_movez# = 0
	
    EndIf

    cam_viewport_yaw# = 180 + cam_viewport_yaw#
    cam_posx# = cam_posx# + cam_movex# * cos_tb(cam_viewport_yaw#) + cam_movez# * sin_tb(cam_viewport_yaw#) 
    cam_posy# = cam_posy# + cam_movey#
    cam_posz# = cam_posz# + cam_movex# * sin_tb(cam_viewport_yaw#) - cam_movez# * cos_tb(cam_viewport_yaw#) 

    PositionEntity camera, cam_posx#, cam_posy#, cam_posz#

    ; ******************************************* Gravity and hitting walls and shit...

	If EntityCollided(Camera,3) Then
	    If cam_movey# > 0 And CollisionY(camera,1) Then
	       cam_movey# = - cam_movey#
  		   MoveEntity Camera,0,cam_movey#,0
        Else
           groundglue# = 1
           cam_movey#  = 0
	    EndIf
;	    ResetEntity Camera
    End If
    If groundglue#=0 Then
        If cam_movey# > -2 Then
		   cam_movey# = cam_movey# - 0.1
		EndIf
    EndIf


End Function


;----------------------------------------------------------------------------------------
Function RotatingSphere()

   PositionEntity rotating_sphere, sin_tb#(scrollcycle) * 500, 500 + cos_tb#(scrollcycle) * 300, cos_tb#(scrollcycle) * 500

End Function


;----------------------------------------------------------------------------------------
Function SetLiftingSphere(i)

   sphere_255# = 310 - sphere_height# (i)
   If sphere_255# = 0 Then
      If out_of_body <> i Then
         sphere_direction#(i) = -1
      EndIf
   ElseIf sphere_255# = 255 Then
      sphere_direction#(i) = .25
   Else
      sphere_direction#(i) = -sphere_direction#(i)
   EndIf
End Function


;----------------------------------------------------------------------------------------
Function SphereAnimate()
       For i = 1 To 4

          sphere_height# (i) = sphere_height# (i) + sphere_direction# (i)
          sphere_255# = 310 - sphere_height# (i)
          If sphere_255# >= 255
             sphere_direction#(i) = 0
             sphere_height# (i) = 55
          EndIf
          If sphere_255# <= 0 Then
             sphere_direction#(i) = 0
             sphere_height# (i) = 310
          EndIf

          ControlSphere(i, sphere_255#)

           If i = 1 And sphere_direction#(i) <> 0 Then
              PositionEntity sphere1,500,sphere_height#(i),500
              EntityColor sphere1,255,sphere_255,sphere_255

           ElseIf i = 2 And sphere_direction#(i) <> 0 Then
              PositionEntity sphere2,-500,sphere_height#(i),500
              EntityColor sphere2,sphere_255,255,sphere_255

           ElseIf i = 3 And sphere_direction#(i) <> 0 Then
              PositionEntity sphere3,-500,sphere_height#(i),-500
              EntityColor sphere3,sphere_255,sphere_255,255

           ElseIf i = 4 Then
              If sphere_direction#(i) <> 0 Then
                 PositionEntity sphere4,500,sphere_height#(i),-500
              EndIf
              If sphere_255# = 255 Then 
                 EntityColor sphere4,255,255,255
              Else

                 Select scrollcycle/120
                   Case 0
                    EntityColor sphere4,sphere_255,sphere_255,255
                   Case 3
                    EntityColor sphere4,sphere_255,sphere_255,255
                   Case 1
                    EntityColor sphere4,sphere_255,255,sphere_255
                   Case 2
                    EntityColor sphere4,255,sphere_255,sphere_255
                 End Select

              EndIf
           EndIf

       Next

End Function


;----------------------------------------------------------------------------------------
Function ControlSphere(i, sphere_255#)

          If sphere_255# = 225 Then
             If sphere_direction# (i) = .5 Then sphere_direction# (i) = 1
             If sphere_direction# (i) = -1 Then sphere_direction# (i) = -.5

          ElseIf sphere_255# = 240 Then
             If sphere_direction# (i) = .25 Then sphere_direction# (i) = .5
             If sphere_direction# (i) = -.5 Then sphere_direction# (i) = -.25
          EndIf

End Function


;----------------------------------------------------------------------------------------
Function AnimatingStarfield()
;Type star_info
;  Field hand
;  Field xpos#
;  Field ypos#
;  Field zpos#
;  Field velocity#
;End Type

   weirdout1 = Rand(1,1000)
   weirdout2 = Rand(1,4000)
   xpos#=EntityX#(camera)
   ypos#=EntityY#(camera)
   zpos#=EntityZ#(camera)


   AddStar(xpos#,ypos#,zpos#,weirdout1, weirdout2)

   For star.star_info = Each star_info

       star\xpos# = star\xpos + star\velocity

       If (star\xpos#) > (xpos# + weirdout1 + 2000) Then
          FreeEntity star\hand
          Delete star.star_info

       Else
          PositionEntity star\hand, star\xpos#, star\ypos#, star\zpos#
       EndIf

   Next

End Function


;----------------------------------------------------------------------------------------
Function AddStar(xref#,yref#,zref#,extrashit1, extrashit2)
      starsize#=Rnd(0.5,2.01)
      speed=Rand(3,15)

      If starsize# > 2 Then starsize# = 18 - speed

      star.star_info=New star_info
      star\hand=CreateSphere (2 + Int starsize#)
      star\xpos#=xref - extrashit1 - 2000
      star\zpos#=zref - extrashit2 + 2000
      star\ypos#=700 + Rand(1,200) 
      star\velocity#=speed
      EntityFX star\hand,1
      ScaleEntity star\hand,starsize,starsize,starsize
      EntityColor star\hand,255,255,255

End Function


;----------------------------------------------------------------------------------------
Function makemaze_3D(x0,y0,x1,y1,x2,y2,scale#)
   viewing_zone = 24
 ;  maze(sx,sy)
 ;  Dim maze_element_exists (viewing_zone * 2, viewing_zone * 2)
   player_x = x0 - Int ((cam_posx# - 0.5) / scale#) 
   player_y = y0 - Int ((cam_posz# - 0.5) / scale#)

   If x1 < (player_x - viewing_zone) Then
      x_start = (player_x - viewing_zone)
   Else
      x_start = x1
   EndIf

   If x2 > (player_x + viewing_zone) Then
      x_end = (player_x + viewing_zone)
   Else
      x_end = x2
   EndIf

   If y1 < (player_y - viewing_zone) Then
      y_start = (player_y - viewing_zone)
   Else
      y_start = y1
   EndIf

   If y2 > (player_y + viewing_zone) Then
      y_end = (player_y + viewing_zone)
   Else
      y_end = y2
   EndIf

   For themaze.cube_info_grid = Each cube_info_grid
       If (in_range (x_start, y_start, x_end, y_end, themaze\mx, themaze\my) = 0) Or (out_of_body > 0) Then
          If maze(themaze\mx, themaze\my) = 255 Then
             maze(themaze\mx, themaze\my) = 0
          EndIf
          FreeEntity themaze\hand
          Delete themaze.cube_info_grid
       EndIf
   Next

   If out_of_body = 0 Then 
      For y = y_start To y_end
         For x = x_start To x_end
            If maze(x,y) = 0 Then
               make_wall(x0,y0,x,y,scale#,viewing_zone)
               maze(x,y) = 255
            EndIf
         Next
      Next
   EndIf

End Function


; retourne 1 si x,y est entre x1,y1 et x2,y2
Function in_range (x1,y1,x2,y2,x,y)
   If x > x1 And x < x2 Then
      If y > y1 And y < y2 Then
         Return 1
      EndIf
   EndIf
   Return 0
End Function


; Puts a cube in the 3D world
Function make_wall(x0,y0,x1,y1,scale#,range)
         themaze.cube_info_grid = New cube_info_grid
         themaze\xpos# = scale# * (x0 - x1)
         themaze\ypos# = scale# * 10
         themaze\zpos# = scale# * (y0 - y1)
         themaze\mx = x1
         themaze\my = y1
         themaze\hand = CreateCube ()
         EntityType themaze\hand,3

     ;    EntityAutoFade themaze\hand, scale# * range /2 , scale# * range 

         ScaleEntity themaze\hand,scale# / 2,scale# * 10,scale# / 2
         EntityFX themaze\hand,1
         EntityTexture themaze\hand,fxtexture(11),0,1
         EntityShininess themaze\hand,.5
         EntityColor themaze\hand,255,255,255
         PositionEntity themaze\hand, themaze\xpos#, themaze\ypos#, themaze\zpos#
End Function



;###############################################
Function init_maze(mx,my,exact=0)
    Local cube1, cube2, cube3, cube4, cube5

    SeedRnd MilliSecs()

;	If exact=1 Then If mx/2.0*2<>mx Or mx/2.0*2<>mx Then RuntimeError "maze size must be odd for exact"
	Dim maze(mx+1,my+1)

    corner_size = 64
    If mx < 205 And my < 205 Then corner_size = 32
    If mx < 169 And my < 169 Then corner_size = 16
    If mx < 135 And my < 135 Then corner_size = 8

    maze_fill(corner_size,Int (corner_size/2) + 1,Int (corner_size/2) + 1)
    maze_fill(corner_size,Int (corner_size/2) + 1,my - Int (corner_size/2))
    maze_fill(corner_size,mx - Int (corner_size/2),Int (corner_size/2) + 1)
    maze_fill(corner_size,mx - Int (corner_size/2),my - Int (corner_size/2))

	For fx=1 To mx: set_maze(fx,1,1,0): set_maze(fx,my,1,0): Next
	For fy=1 To my: set_maze(1,fy,1,0): set_maze(mx,fy,1,0): Next
  ;  For fx=0 To (mx+1): set_maze(fx,0,0,0): set_maze(fx,my+1,0,0): Next
	;For fy=0 To (my+1): set_maze(0,fy,0,0): set_maze(mx+1,fy,0,0): Next

	farx=mx
	fary=my
	SeedRnd MilliSecs()
	ally=1:allx=1
	exact=exact+1

	Repeat
		Repeat
			allx=allx+getcos(alldir)*exact: ally=ally+getsin(alldir)*exact
			go=go+exact
			If go>=fary-exact And (alldir=1 Or alldir=3) Then go=0:fary=fary-exact:alldir=wrap4(alldir+1)
			If go>=farx-exact And (alldir=0 Or alldir=2) Then go=0:farx=farx-exact:alldir=wrap4(alldir+1)
		Until (maze(allx,ally)>0 And (allx<(mx-1) Or ally<(my-1))) Or (farx<0 Or fary<0)
		If farx<0 Or fary<0 Then Exit

		If allx>mx Then If ally>my Then Exit
		x=allx
		y=ally
		dir=Rand(0,3)
		Repeat
			For f=0 To 3
				If cango(f,mx,my,x,y,exact) = 1 Then Exit
			Next
			If f=4 Then Exit
			dir=wrap4(dir+Rand(-1,+1))
			If cango(dir,mx,my,x,y,exact) = 1 Then 

			   For f=1 To exact
				  x=x+getcos(dir)
				  y=y+getsin(dir)

                  If sx Mod 7 And sy Mod 3
                     ; size 16 boxes in corner
		             If plot_in_range(16,75,75,x,y) And cube1 = 0 Then
		                cube1 = 1
		                maze_fill(16,75,75)
		             ElseIf plot_in_range(16,mx + 1 - 75,75,x,y) And cube2 = 0 Then
				        cube2 = 1
		                maze_fill(16,mx + 1 - 75,75)
		             ElseIf plot_in_range(16,75,my + 1 - 75,x,y) And cube3 = 0 Then
		   		        cube3 = 1
		                maze_fill(16,75,my + 1 - 75)
		             ElseIf plot_in_range(16,mx + 1 - 75,my + 1 - 75,x,y) And cube4 = 0 Then
				        cube4 = 1
		                maze_fill(16,mx + 1 - 75,my + 1 - 75)
		             ; the big size 64 box in the center (always)
		             ElseIf plot_in_range(64,(mx+1)/2,(my+1)/2,x,y) And cube5 = 0 Then
				        cube5 = 1
                        maze_fill(64,(mx+1)/2,(my+1)/2)
		             Else
		                set_maze(x,y,1,1)
		             EndIf
		          Else
		             ; size 16 boxes in middle sides
		             If plot_in_range(16,mx/2+1,my/4,x,y) And cube1 = 0 Then
		                cube1 = 1
		                maze_fill(16,mx/2+1,my/4)
		             ElseIf plot_in_range(16,mx/4,my/2+1,x,y) And cube2 = 0 Then
				        cube2 = 1
		                maze_fill(16,mx/4,my/2+1)
		             ElseIf plot_in_range(16,mx/2+1,my - my/4 + 1,x,y) And cube3 = 0 Then
		   		        cube3 = 1
		                maze_fill(16,mx/2+1,my - my/4 + 1)
		             ElseIf plot_in_range(16,mx - mx/4 - 1,my/2+1,x,y) And cube4 = 0 Then
				        cube4 = 1
		                maze_fill(16,mx - mx/4 - 1,my/2+1)
				     ; the big size 64 box in the center (always)
		             ElseIf plot_in_range(64,(mx+1)/2,(my+1)/2,x,y) And cube5 = 0 Then
				        cube5 = 1
                        maze_fill(64,(mx+1)/2,(my+1)/2)
		             Else
		                set_maze(x,y,1,1)
		             EndIf

		          EndIf
		
		
			   Next

			EndIf
		Forever
	Forever

End Function

Function maze_fill(size, center_pos_x, center_pos_y)
	For fx = (center_pos_x - Int (size/2)) To (center_pos_x + Int (size/2))
       For fy = (center_pos_y - Int (size/2)) To (center_pos_y + Int (size/2))
          set_maze(fx,fy,1,0)
       Next
    Next
End Function


Function plot_in_range (size, center_pos_x, center_pos_y, x, y)
   If x >= (center_pos_x - Int (size/2)) And x <= (center_pos_x + Int (size/2)) Then
      If y >= (center_pos_y - Int (size/2)) And y <= (center_pos_y + Int (size/2))
         Return 1
      EndIf
   EndIf
   Return 0
End Function


Function cango(dir,mx,my,x,y,exact)
	For f=1 To exact+1
		x2=x+getcos(dir)*f: y2=y+getsin(dir)*f
		If x2<=0 Or y2<=0 Or x2>mx Or y2>my Then Return 0
		If maze(x2,y2)>0 Then Return 0
		If maze(x2+getcos(wrap4(dir+1)),y2+getsin(wrap4(dir+1)))>0 Then Return 0
		If maze(x2+getcos(wrap4(dir-1)),y2+getsin(wrap4(dir-1)))>0 Then Return 0
	Next
	Return 1
End Function


Function wrap4(value)
	Return value -4*(value>3) +4*(value<0)
End Function


Function set_maze(x,y,fx,s)
    If maze(x,y) <> fx Then
    	maze(x,y)=fx
        If s = 1 Then For i=1 To Rand(69,8192):SeedRnd MilliSecs():Next
        maze_boxes = maze_boxes + 1

        If test_layout Then 
	       plot_maze (x,y,fx,4)
           Color 0,0,0
           Rect 1200,0,50,12
           Color 255,255,255
           Text 1200,0,maze_boxes
        EndIf
    EndIf
End Function


Function plot_maze (x,y,fx,z)
		If fx=1 Then Color 255,255,255 : Rect x*z,y*z,z,z
		If fx=2 Then Color 0,255,255 : Rect x*z,y*z,z,z
		If fx=3 Then Color 255,0,255 : Rect x*z,y*z,z,z
		If fx=4 Then Color 255,255,0 : Rect x*z,y*z,z,z
		If fx=5 Then Color 0,0,255 : Rect x*z,y*z,z,z
		If fx=6 Then Color 255,0,0 : Rect x*z,y*z,z,z
		If fx=7 Then Color 0,255,0 : Rect x*z,y*z,z,z
End Function



;----------------------------------------------------------------------------------------
Function WallUnit()

    If Rand(0,100) = 1 Then
       If Rand(1,2) = 1 Then
          WallUnitSeed(1,1)
          WallCopySeed(1,2)
          WallCopySeed(1,3)
          WallCopySeed(1,4)
       Else
          WallUnitSeed(1,1)
          WallUnitSeed(2,1)
          WallUnitSeed(3,1)
          WallUnitSeed(4,1)
       EndIf

       WallUnitHandle()

    EndIf

    If Rand(0,100) = 1 Then
       WallUnitSeed(5,2)

       WallUnitHandle()
    EndIf

	fx_u# = fx_u + fx_direction#
	fx_v# = fx_v - fx_direction#

    If fx_u# > 0.2 Or fx_u# < 0.01
	   If fx_direction# = + 0.001
	      fx_direction# = - 0.001
	   Else
	      fx_direction# = + 0.001
	   EndIf
    EndIf

	; Rotate, move and scale texture on the box...

    For i = 1 To 5
       TextureAnimateHandle(walleffect(i), wall_texture_number(i))
    Next

    RotateWallUnit()


End Function


;----------------------------------------------------------------------------------------
Function WallUnitSeed(wallnumber, walltype)
       If walltype = 1 Then
          wall_texture_number(wallnumber) = Rand(1,4)
       Else
          wall_texture_number(wallnumber) = Rand(5,8)
       EndIf

       walleffect(wallnumber) = Rand(1,8)
       wallfxzoom(wallnumber) = Rand(1,5) * 4
       walldisplace#(wallnumber) = Rand(0.01,0.05)
End Function


;----------------------------------------------------------------------------------------
Function WallCopySeed(wallsource, walldest)
       wall_texture_number(walldest) = wall_texture_number(wallsource)

       walleffect(walldest) = walleffect(wallsource) 
       wallfxzoom(walldest) = walleffect(wallsource)
       walldisplace#(walldest) = walldisplace#(wallsource)
End Function


;----------------------------------------------------------------------------------------
Function SetRotatingWall(wall)
       If wall_angle(wall) = 0 Then
          wall_rotate(wall) = 1
       Else
          wall_rotate(wall) = -1
       EndIf

End Function

;----------------------------------------------------------------------------------------
Function TextureAnimateHandle(walleffect, walltexture)

    If walleffect = 1 Then
	   RotateTexture fxtexture(walltexture), fx_u * 100 * wallfxzoom(walltexture)
	   PositionTexture fxtexture(walltexture), fx_u * wallfxzoom(walltexture), fx_v * wallfxzoom(walltexture)
	   ScaleTexture fxtexture(walltexture), fx_u + walldisplace#(walltexture) , fx_v + walldisplace#(walltexture)

    ElseIf walleffect = 2 Then
	   RotateTexture fxtexture(walltexture), fx_v * 100 * wallfxzoom(walltexture)
	   PositionTexture fxtexture(walltexture), fx_v * wallfxzoom(walltexture), fx_u * wallfxzoom(walltexture)
	   ScaleTexture fxtexture(walltexture), fx_v + walldisplace#(walltexture) , fx_u + walldisplace#(walltexture)
	
	ElseIf walleffect = 3 Then
	   PositionTexture fxtexture(walltexture), fx_u * wallfxzoom(walltexture), fx_u * wallfxzoom(walltexture)
	   ScaleTexture fxtexture(walltexture), fx_u + walldisplace#(walltexture) , fx_u + walldisplace#(walltexture)

	
	ElseIf walleffect = 4 Then
	   PositionTexture fxtexture(walltexture), fx_v * wallfxzoom(walltexture), fx_v * wallfxzoom(walltexture)
	   ScaleTexture fxtexture(walltexture), fx_v + walldisplace#(walltexture) , fx_v + walldisplace#(walltexture)

	ElseIf walleffect = 5 Then
	   RotateTexture fxtexture(walltexture), fx_u * 100 * wallfxzoom(walltexture)
	
	ElseIf walleffect = 6 Then
	   RotateTexture fxtexture(walltexture), fx_v * 10 * wallfxzoom(walltexture)
	
	ElseIf walleffect = 7 Then
	   ScaleTexture fxtexture(walltexture), cos_tb#(scrollcycle * fx_u) , sin_tb#(scrollcycle * fx_v)
	   RotateTexture fxtexture(walltexture), scrollcycle * wallfxzoom(walltexture)

	Else
	   PositionTexture fxtexture(walltexture), sin_tb#(scrollcycle) ,cos_tb#(scrollcycle)
	EndIf

End Function


;----------------------------------------------------------------------------------------
Function UpdateTexture(texnumber)
  SetBuffer ImageBuffer(texture_buffer)
  LockBuffer ImageBuffer(texture_buffer)
  For i=1 To Rnd(0,16)                             ;Add random number of pixels in random colors
      r=Rnd(0,255)
      g=Rnd(0,255)
      b=Rnd(0,255)
      WritePixelFast Rnd(0,64),33,r Shl 8+g Shl 24+b
  Next
  For x=0 To 64                                    ;Recalculate color if every pixel from upper half of texture from color-
    For y=33 To 0 Step -1                          ;values of itself and its x-neighbours
      heinz= ( ( ReadPixelFast (x-1,y) + ReadPixelFast (x+1,y)) / 2 + ReadPixelFast (x,y)) / 2
      WritePixelFast x,y-1,heinz                   ;Write pixel in upper half
      WritePixelFast x,67-y,heinz                  ;...and lower half
    Next
  Next
  UnlockBuffer ImageBuffer(texture_buffer)
  SetBuffer TextureBuffer(fxtexture(texnumber))           ;Finally blit the texture image into texture
  DrawBlockRect texture_buffer,0,0,0,0,64,32              ;but leave out the four pixel-rows in the middle for
  DrawBlockRect texture_buffer,0,32,0,36,64,32            ;better look. Just try DrawBlock texture,0,0 to compare

  SetBuffer BackBuffer()

End Function                                       


;----------------------------------------------------------------------------------------
Function WallUnitHandle()

       For wall.object_info = Each object_info
          FreeEntity wall\hand
          Delete wall.object_info
       Next

       For i = 1 To 5
          wall.object_info=New object_info
          wall\xpos#=0
          wall\ypos#=0
          wall\hand=CreateCube ()

          EntityTexture wall\hand,fxtexture(wall_texture_number(i)),0,1
          EntityFX wall\hand,1
          EntityType wall\hand,3

          If i = 1 Then
             ScaleEntity wall\hand,200,100,1
             PositionEntity wall\hand, 0, 100, +200

          ElseIf i = 2 Then
             ScaleEntity wall\hand,1,100,200
             PositionEntity wall\hand, -200, 100, 0

          ElseIf i = 3 Then
             ScaleEntity wall\hand,200,100,1
             PositionEntity wall\hand, 0, 100, -200

          ElseIf i = 4 Then
             ScaleEntity wall\hand,1,100,200
             PositionEntity wall\hand, +200, 100, 0

          Else
             ScaleEntity wall\hand,200,1,200
             PositionEntity wall\hand, 0, 200, 00

          EndIf

       Next

End Function


;----------------------------------------------------------------------------------------
Function RotateWallUnit()

       i = 1

       For wall.object_info = Each object_info
          wall_angle (i) = wall_angle (i) + wall_rotate (i)

          If wall_angle (i) = 180 Or wall_angle (i) = 0 Then
             wall_rotate(i) = 0
          EndIf
 
          If i = 1 
             RotateEntity wall\hand,-wall_angle(i),0,0
             PositionEntity wall\hand,0, 100, 200 + wall_angle(i)
 
          ElseIf i = 2 Then
             RotateEntity wall\hand,0,0,wall_angle(i)
             PositionEntity wall\hand, -200- wall_angle(i) , 100, 0 

          ElseIf i = 3 Then
             RotateEntity wall\hand,wall_angle(i),0,0
             PositionEntity wall\hand, 0, 100, -200 - wall_angle(i)

          ElseIf i = 4 Then
             RotateEntity wall\hand,0,0,-wall_angle(i)
             PositionEntity wall\hand, 200 + wall_angle(i), 100, 0

     ;     ElseIf i = 5 Then
     ;        RotateEntity wall\hand,0,wall_angle(i),0
     ;        PositionEntity wall\hand, 0, 200, 00

           EndIf

          i = i + 1

       Next

End Function


;----------------------------------------------------------------------------------------
Function one_door_open()
   If wall_angle (1) > 0 Then Return 1
   If wall_angle (2) > 0 Then Return 1
   If wall_angle (3) > 0 Then Return 1
   If wall_angle (4) > 0 Then Return 1
   Return 0
End Function


;----------------------------------------------------------------------------------------
Function CloseAllWalls()
      If wall_angle (1) > 0 Then wall_rotate(1) = -1
      If wall_angle (2) > 0 Then wall_rotate(2) = -1
      If wall_angle (3) > 0 Then wall_rotate(3) = -1
      If wall_angle (4) > 0 Then wall_rotate(4) = -1
End Function


;----------------------------------------------------------------------------------------
Function SetupWorld()

   MoveMouse GraphicsWidth()/2, GraphicsHeight()/2

   MakeCubeRoom()


   WallUnitSeed(1,1)
   WallCopySeed(1,2)
   WallCopySeed(1,3)
   WallCopySeed(1,4)
   WallUnitSeed(5,2)

   WallUnitHandle()

End Function


;----------------------------------------------------------------------------------------
Function CubeRoomHandle()

    If cuberoom = 1 Then CubeScroller()

    If far_from_room() Then CloseAllWalls()

    If in_center_room() Or (one_door_open() = 1 And in_center_room() = 0) Then
       If cuberoom = 0 Then
          MakeCubeRoom()
       EndIf
    Else
       If cuberoom = 1 Then
          DeleteCubeRoom()
       EndIf
    EndIf

End Function


;----------------------------------------------------------------------------------------
Function CubeScroller()
  newscrollitem = newscrollitem + 1

  If newscrollitem > 24                            ;Is it time for engaging the next scrollcube?
    char=Asc(Mid$(scrolltext$,sc_offset,1))-32         ;compute ASCIIcode of relevant character into Array-Index for letter(X)
    If char >= 0 And char < 96 And char <> 0       ;create new cube object and initialize its fields.
      sci.cube_info=New cube_info
      sci\xpos#=+175
      sci\ypos#=0
      sci\hand=CreateCube ()
      EntityType sci\hand,3

      EntityFX sci\hand,1

      ScaleEntity sci\hand,10,10,10
      EntityTexture sci\hand,letter(char),0,1      ;Assign appropriate texture for relevant character
      EntityTexture sci\hand,fxtexture(10)         ;...just as the color-texture
    EndIf
    newscrollitem=0                                ;reset delaycounter
    sc_offset=sc_offset+1                          ;update relevant character of scrolltext$
    If sc_offset > Len(scrolltext$) Then sc_offset=1   ;wrap scrolltext if ended
  EndIf

  For sci.cube_info=Each cube_info                 ;Update entity-positions And Delete If necessary...
    sci\xpos#=sci\xpos# - 1
    If sci\xpos#<-175                                
      FreeEntity sci\hand
      Delete sci.cube_info
    Else
      sci\ypos#=sin_tb#(720 + sci\xpos# - scrollcycle) * 50
      PositionEntity sci\hand,sci\xpos#,100 + sci\ypos#,150
      TurnEntity sci\hand,.1,.3,0
      EntityAlpha sci\hand,(175 - Abs sci\xpos#)/35
    EndIf
  Next

End Function



;----------------------------------------------------------------------------------------
Function MakeCubeRoom()
   ; Let's put all those cubes
   For x= -50 To +50 Step 10
      For y= 0 To +100 Step 10
         For z= -50 To +50 Step 10

            If Rand(0,4) = 2 Then
               matrice.cube_info2 = New cube_info2
               matrice\xpos# = x
               matrice\ypos# = y
               matrice\zpos# = z 
               matrice\hand=CreateCube ()
               ScaleEntity matrice\hand,1,1,1

               EntityType matrice\hand,3
               EntityTexture matrice\hand,letter(Rand(1,95)),0,1
               EntityFX matrice\hand,3
               EntityShininess matrice\hand,1
               PositionEntity matrice\hand, matrice\xpos#, matrice\ypos#+1, matrice\zpos#

            EndIf

         Next 
      Next 
   Next 
   cuberoom = 1
End Function


;----------------------------------------------------------------------------------------
Function DeleteCubeRoom()
  For matrice.cube_info2=Each cube_info2     ;Update entity-positions And Delete If necessary...
      FreeEntity matrice\hand
      Delete matrice.cube_info2
  Next
  cuberoom = 0
End Function




;----------------------------------------------------------------------------------------
Function SetGfx()
    Graphics 640,480,0,2

    windowed_mode = 1  ; 1 = not windowed, 2 = windowed
    antialias_mode = False

	cnt=CountGfxModes3D()
	If cnt=0 Print "No 3D Graphics modes detected, exiting...":WaitKey:End

	If Windowed3D()
		yn$=Input$( "Use windowed mode?" )
		If Left$( Lower$( yn$ ),1 )="y"
			windowed_mode = 2
		EndIf
	EndIf
	
	Print ""
	Print "Display drivers:"
	Print "----------------"
	For k=1 To CountGfxDrivers()
		Print k+":"+GfxDriverName$(k)
	Next
	Print
	
	If CountGfxDrivers()>1
		Repeat
			driver=Input$( "Display driver (1-"+CountGfxDrivers()+"):" )
		Until driver>=1 And driver<=CountGfxDrivers()
		SetGfxDriver driver
	EndIf
	
	Print ""
	Print "Display modes:"
	Print "--------------"
	liste$=""
	For k=1 To CountGfxModes3D()
		liste$=liste$+LSet$(k+":"+GfxModeWidth(k)+","+GfxModeHeight(k)+","+GfxModeDepth(k),20)
		If Len(liste$) = 80 Then
		   Print liste$
		   liste$ = ""
		EndIf
	Next
	Print liste$
	
	Repeat
		mode=Input$( "Display Mode (1-"+cnt+"):" )
	Until mode>=1 And mode<=cnt
	
	
	yn$=Input$( "Use antialias?" )
	If Left$( Lower$( yn$ ),1 )="y"
		antialias_mode = True
	EndIf

	
	Graphics3D GfxModeWidth(mode),GfxModeHeight(mode),GfxModeDepth(mode),windowed_mode
	AntiAlias antialias_mode

    HidePointer
    FlushMouse
    HWMultiTex True

End Function
