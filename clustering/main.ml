open Util

let kmeans
    dist
    make_centroid
    ?(initialize_centroids = Array.select_rand)
    ?(term_iter = 1000)
    ?(term_error = 10e-12)
    points
    n
    =
  let select_cluster centroids point = Array.find_min_num (dist point) centroids
  and filter_cluster clusters =
    let result = Array.make n [] in
    let _ = Array.iteri (fun j c -> result.(c) <- points.(j) :: result.(c)) clusters in
    result
  in
  let rec aux iter clusters old_centroids =
    let new_centroids = Array.map make_centroid (filter_cluster clusters) in
    let clusters = Array.map (select_cluster new_centroids) points in
    let error = Array.map2 dist new_centroids old_centroids |> Array.find_max identity in
    if error < term_error || iter > term_iter then clusters
    else aux (iter + 1) clusters new_centroids
  in
  let init_centroids = initialize_centroids n points in
  let init_clusters = Array.map (select_cluster init_centroids) points in
  aux 0 init_clusters init_centroids
