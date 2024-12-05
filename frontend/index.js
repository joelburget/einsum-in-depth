import * as isometric from '@elchininet/isometric';
window.isometric = isometric;
import cytoscape from 'cytoscape';
window.cytoscape = cytoscape;
import fcose from 'cytoscape-fcose';
cytoscape.use( fcose );
import './tensor_playground_web';

window.addEventListener('load', () => window.tensor_playground_main());
