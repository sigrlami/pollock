import * as React from 'react';
import { Navbar, Nav, NavItem, Glyphicon } from 'react-bootstrap';
import { RouterOnContext } from 'react-router';
import { LinkContainer } from 'react-router-bootstrap';
import { bind } from 'decko';
import * as s from './styles.styl';
import * as block from 'bem-cn';
import SyntheticEvent = React.SyntheticEvent;

interface IProps {
  children?: React.ReactNode;
}

interface IContext {
  router: RouterOnContext;
}

class Header extends React.Component<IProps, {}> {
  public static contextTypes = {
    router: React.PropTypes.object,
  };
  public context: IContext;
  private b = block('header');

  public render() {
    const b = this.b;
    const { children } = this.props;

    return (
      <Navbar>
        <Navbar.Header>
          <Navbar.Brand className={s[b('brand')()]}>
            Pollok
          </Navbar.Brand>
        </Navbar.Header>
        <Nav>
          <LinkContainer to="/questions/list" onlyActiveOnIndex>
            <NavItem>
              <Glyphicon glyph="align-left" />
            </NavItem>
          </LinkContainer>
          <LinkContainer to="/questions/create" onlyActiveOnIndex>
            <NavItem>
              <Glyphicon glyph="plus" />
            </NavItem>
          </LinkContainer>
        </Nav>
        {children}
      </Navbar>
    );
  }
}

export { IProps };
export default Header;
