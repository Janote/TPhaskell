
type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-----------------------------------------------------------------

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs
-- En la función Pertenece, verificamos si el elemento e es igual al primer elemento de la secuencia, si no lo es, 
-- llamamos a Pertenece recursivamente con el resto de la secuencia.

mismosElementos :: Eq a => [a] -> [a] -> Bool
mismosElementos xs ys = length xs == length ys && mismosElementosAux xs ys
-- La función mismosElementos compara si dos listas tienen los mismos elementos, 
-- independientemente del orden en que aparecen en cada lista.
mismosElementosAux :: Eq a => [a] -> [a] -> Bool
mismosElementosAux [] [] = True
mismosElementosAux [] _ = False
mismosElementosAux _ [] = False
mismosElementosAux (x:xs) ys = pertenece x ys && mismosElementosAux xs ys
-- mismosElementosAux para verificar que todos los elementos de una lista estén contenidos en la otra. 
-- Esta función auxiliar     compara recursivamente cada elemento de la primera lista con los de la segunda lista y va eliminando los elementos coincidentes. 

iguales :: Eq a => [Relacion] -> Bool
iguales [] = True
iguales [_] = True
iguales ((a,b):(c,d):rs) = (a == c && b == d) && iguales ((c,d):rs)




-- La función verifica si cada relación en la lista es asimétrica, 
-- es decir, si no existe una relación en la lista que tenga los mismos elementos pero en orden invertido.

usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ [] = True
usuariosDeRelacionValidos us ((u1, u2):rs) = pertenece u1 us && pertenece u2 us && u1 /= u2 && usuariosDeRelacionValidos us rs
-- recibe una lista de usuarios y una lista de relaciones, 
-- y devuelve un valor booleano que indica si todos los usuarios involucrados en las relaciones están en la lista de usuarios 
-- y si ninguna relación tiene usuarios duplicados. 

sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed _ [] = True
sonDeLaRed red (u:us) = pertenece u (usuarios red) && sonDeLaRed red us
-- retorna True si se llega al final de la lista sin encontrar ningún usuario que no pertenezca a la red.

empiezaCon :: Eq a => a -> [a] -> Bool
empiezaCon _ [] = False
empiezaCon e (x:xs) = e == x
-- toma un elemento e y una lista xs 
-- y devuelve un valor booleano que indica si la lista xs comienza con el elemento e.

terminaCon :: Eq a => a -> [a] -> Bool
terminaCon e [] = False 
terminaCon e (x:xs) | e == last xs = True
                    | otherwise = False


-- toma un elemento e y una lista xs, 
-- y devuelve True si la lista xs termina con el elemento e, y False en caso contrario.

sinRepetidos :: Eq a => [a] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) = x `notElem` xs && sinRepetidos xs
-- verifica si una lista no tiene elementos repetidos.

usuarioValido :: Usuario -> Bool
usuarioValido u = idDeUsuario u > 0 && length (nombreDeUsuario u) > 0

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (u:us) = idDeUsuario u `notElem` map idDeUsuario us && noHayIdsRepetidos us
-- toma una lista de usuarios y verifica si no hay dos usuarios con el mismo ID de usuario.
-- Si en algún momento se encuentra un usuario con un ID de usuario que ya está en la lista, 
-- se devuelve False y se detiene la recursión.
    
usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (u:us) = usuarioValido u && noHayIdsRepetidos (u:us) && usuariosValidos us
-- verifica que todos los usuarios en la lista dada sean válidos 
-- y que no haya ids de usuario repetidos.
-- Luego, para cada usuario en la lista, verifica si el usuario es válido utilizando la función usuarioValido. 
-- También verifica si no hay ids de usuario repetidos en la lista completa, 
-- utilizando la función noHayIdsRepetidos. 


