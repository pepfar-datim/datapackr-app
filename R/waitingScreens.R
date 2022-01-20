waiting_screen_datapack <- tagList(
  waiter::spin_hourglass(),
  h4("Generating your SNUxIM tab. Please wait...")
)

waiting_screen_comparison <- tagList(
  waiter::spin_hourglass(),
  h4("Generating a comparison to DATIM. Please wait...")
)

waiting_screen_flatpack <- tagList(
  waiter::spin_hourglass(),
  h4("Generating your FlatPack. Please wait...")
)

waiting_screen_paw <- tagList(
  waiter::spin_ring(),
  h4("Transferring files to PAW. Please wait...")
)
