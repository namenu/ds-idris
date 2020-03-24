module Material

import Color
import Light
import Tuple

%access export

record Material where
  constructor MkMaterial
  color : Color
  ambient, diffuse, specular, shininess : Double


defaultMaterial : Material
defaultMaterial = MkMaterial (MkColor 1 1 1)
                             0.1
                             0.9
                             0.9
                             200.0


lighting : Light -> Tuple -> Tuple -> Tuple -> Material -> Color
lighting (MkLight light_position light_intensity)
         point
         eyev
         normalv
         (MkMaterial mat_color mat_ambient mat_diffuse mat_specular mat_shininess)
  = let effective_color = mat_color * light_intensity
        lightv = normalize (light_position - point)
        ambient = effective_color *. mat_ambient

        light_dot_normal = dot lightv normalv

        (diffuse, specular)
          = if light_dot_normal < 0
              then (black, black)
              else
                let diffuse = effective_color *. (mat_diffuse * light_dot_normal)
                    reflectv = reflect (neg lightv) normalv
                    reflect_dot_eye = dot reflectv eyev

                    specular = if reflect_dot_eye <= 0
                                 then black
                                 else let factor = pow reflect_dot_eye mat_shininess
                                       in light_intensity *. mat_specular * factor
                 in (diffuse, specular)

      in ambient + diffuse + specular
