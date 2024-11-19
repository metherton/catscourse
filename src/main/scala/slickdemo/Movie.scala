package slickdemo

import java.time.LocalDate

final case class Movie(
                        id: Long,
                        name: String,
                        releaseDate: LocalDate,
                        lengthInMin: Int
                      )

