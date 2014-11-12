/* Colin Woodbury
 * 301238755
 * 2014 November 11 @ 16:01
 *
 * This is a terrible language and it shouldn't be taught.
 * Must be run with: `java -cp "mysql-connector-java-5.1.34/*:." a3`
 */

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class a3 {
        public static void main(String[] args) {
                Connection c = null;

                try {
                        System.out.println("Loading the Driver...");
                        Class.forName("com.mysql.jdbc.Driver").newInstance();
                } catch (Exception ex) {
                        System.out.println("Failed to load the Driver.");
                }

                try {
                        DriverManager
                                .getConnection("jdbc:mysql://localhost/354a2?" +
                                               "user=root&password=r00t");

                        System.out.println("Connection successful.");
                } catch (SQLException ex) {
                        // handle any errors
                        System.out.println("SQLException: " + ex.getMessage());
                        System.out.println("SQLState: " + ex.getSQLState());
                        System.out.println("VendorError: " + ex.getErrorCode());
                }
        }
}
