/****************** SERVER CODE ****************/

#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>

unsigned int ffi_send(unsigned int sock, unsigned int n, char *buffer)
{
  ssize_t ret;

  ret = send(sock,buffer,n,0);

  if(ret < 0 || ret > n)
    {
      exit(-1);
    }
    
  return ret;
}

unsigned int ffi_accept(void)
{
  int welcomeSocket;
  struct sockaddr_in serverAddr;
  struct sockaddr_storage serverStorage;
  socklen_t addr_size;
  int ret;
  
  /*---- Create the socket. The three arguments are: ----*/
  /* 1) Internet domain 2) Stream socket 3) Default protocol (TCP in this case) */
  welcomeSocket = socket(PF_INET, SOCK_STREAM, 0);
  
  /*---- Configure settings of the server address struct ----*/
  /* Address family = Internet */
  serverAddr.sin_family = AF_INET;
  /* Set port number, using htons function to use proper byte order */
  serverAddr.sin_port = htons(7891);
  /* Set IP address to localhost */
  serverAddr.sin_addr.s_addr = inet_addr("127.0.0.1");
  /* Set all bits of the padding field to 0 */
  memset(serverAddr.sin_zero, '\0', sizeof serverAddr.sin_zero);  

  /*---- Bind the address struct to the socket ----*/
  if (!bind(welcomeSocket, (struct sockaddr *) &serverAddr, sizeof(serverAddr)))
    {
      exit(-1);
    }

  /*---- Listen on the socket, with 1 max connection requests queued ----*/
  if(!listen(welcomeSocket,1))
    {
      exit(-1);
    }

  /*---- Accept call creates a new socket for the incoming connection ----*/
  addr_size = sizeof serverStorage;

  ret = accept(welcomeSocket, (struct sockaddr *) &serverStorage, &addr_size);
  if(ret < 0)
    {
      exit(-1);
    }
  return ret;
}

int main()
{
  unsigned int sock = ffi_accept();
  /*---- Send message to the socket of the incoming connection ----*/
  ffi_send(sock, 13, "hello, world");
  ffi_send(sock, 13, "hello, world");

  return 0;
}
